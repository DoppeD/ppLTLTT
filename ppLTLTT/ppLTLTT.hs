{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module Main where

import qualified Data.Map as Map

import Control.Exception      ( try )
import Control.Monad          ( liftM, liftM2, unless )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.State.Strict
                              ( execStateT, get, gets, modify, runStateT, StateT )
import Data.Bits              ( setBit, testBit )
import Data.List              ( intersperse, sortBy )
import System.Exit            ( ExitCode, die )
import System.IO              ( Handle, hClose, hPutStr, hPutStrLn, openFile, stdout, IOMode(..) )

import qualified ArgsPPLTLTT as Args

import Common
import ParserSpot
import PLTL ( PLTL )

import qualified PLTL

type AnnotationIO a = StateT AnnotationState IO a
type PastIO a = StateT PastState IO a

-- We identify states with the subformulae that hold true in them, as per usual, except we only track
-- subformulae with past top-level operators.
type State           = Integer
type Valuation       = State
type ProcessedStates = Map.Map State Int -- A state and its associated index.

-- Prop, Prev, Once, Histor and Since have indices, so that we can get/set the corresponding
-- bit when checking/setting truth values in valuations and states.
data IndexedFormula
    = Prop Int
    | Not IndexedFormula
    | Prev IndexedFormula Int
    | WPrev IndexedFormula Int
    | Once IndexedFormula Int
    | Histor IndexedFormula Int
    | Since IndexedFormula IndexedFormula Int
    | SinceW IndexedFormula IndexedFormula Int
    | And IndexedFormula IndexedFormula
    | Or IndexedFormula IndexedFormula
    | Xor IndexedFormula IndexedFormula
    | Impl IndexedFormula IndexedFormula
    | Iff IndexedFormula IndexedFormula
    | T
    | F
  deriving (Eq, Ord, Show, Read)

data PastState = PastState
  { evaluations         :: Integer
  , formula             :: IndexedFormula
  , nextIndex           :: Int
  , output              :: [String]
  , previousSubformulae :: [(IndexedFormula, Int)]
  , processedStates     :: ProcessedStates
  , variables           :: [(String, Int)]
  }

data AnnotationState = AnnotationState
  { indexedPrevs :: [(IndexedFormula, Int)]
  , prevs        :: Map.Map PLTL Int
  , prevIndex    :: Int
  , varIndex     :: Int
  , vars         :: Map.Map String Int
  }

-- Evaluates if a formula is true now. Anything involving past should be in the state.
-- Anything involving propositions should be in the valuation.
eval :: State -> Valuation -> IndexedFormula -> Bool
eval state val (Prop i) = testBit val i
eval state val (Or f g) = (eval state val f) || (eval state val g)
eval state val (Xor f g) = (eval state val f) /= (eval state val g)
eval state val (And f g) = (eval state val f) && (eval state val g)
eval state val (Not f) = not (eval state val f)
eval state val (Prev _ i) = testBit state i
eval state val (WPrev _ i) = not (testBit state i)
eval state val (Once f i) = (testBit state i) || (eval state val f)
eval state val (Histor f i) = (not (testBit state i)) && (eval state val f)
eval state val (Since f g i) =
  ((testBit state i) && (eval state val f)) || (eval state val g)
eval state val (SinceW f g i) =
  (not (testBit state i) && (eval state val f)) || (eval state val g)
eval state val (Impl f g) = (not (eval state val f)) || (eval state val g)
eval state val (Iff f g) = (eval state val f) == (eval state val g)
eval state val T = True
eval state val F = False

---- Main algorithm ----

writeTransition :: State -> Valuation -> PastIO ((State, Int), Bool)
writeTransition state valuation = do
  PastState { formula, previousSubformulae, variables } <- get
  let nextState = foldl (\n (g, i) -> if (eval state valuation g) then setBit n i else n) 0 previousSubformulae
  (index, shouldCreate) <- assignIndex nextState
  let propValues = fmap (\(p, i) -> if (testBit valuation i) then p else ("!" ++ p)) variables
      formulaValue = if (eval state valuation formula) then "0" else "!0"
      newOutput = concat $ intersperse "&" $ formulaValue : propValues
  modify (\s -> s { output = ("[" ++ newOutput ++ "] " ++ show index) : output s })
  return ((nextState, index), shouldCreate)

-- Assigns a new index to a state, unless it has already been processed, in which case its
-- previously assigned index is returned.
assignIndex :: State -> PastIO (Int, Bool)
assignIndex state = do
  PastState { nextIndex, processedStates } <- get
  case Map.insertLookupWithKey (\_ _ -> id) state nextIndex processedStates of
    (Nothing, newProcessedStates) -> do
      modify (\s -> s { nextIndex = nextIndex + 1, processedStates = newProcessedStates })
      return (nextIndex, True)

    (Just currentIndex, _) ->
      return (currentIndex, False)

processState :: (State, Int) -> PastIO ()
processState (state, index) = do
  PastState { evaluations, formula } <- get
  modify (\s -> s { output = ("State: " ++ show index) : output s })
  reachableStates <- mapM (writeTransition state) ([0..evaluations] :: [Integer])
  let statesToProcess = fmap fst $ filter snd reachableStates
  mapM_ processState statesToProcess

---- Preprocessing ----

initPastState :: IndexedFormula -> [(IndexedFormula, Int)] -> [(String, Int)] -> PastState
initPastState formula prevSubformulae vars = PastState
  { evaluations         = 2^varLen - 1
  , formula             = formula
  , nextIndex           = 1
  , output              = []
  , previousSubformulae = prevSubformulae
  , processedStates     = Map.singleton 0 0
  , variables           = vars
  }
  where varLen = length vars

annotationInitState :: AnnotationState
annotationInitState = AnnotationState
  { indexedPrevs = []
  , prevs        = Map.empty
  , prevIndex    = 0
  , varIndex     = 0
  , vars         = Map.empty
  }

updateOrReadIndex :: Ord a => a -> (AnnotationState -> Map.Map a Int)
                  -> (AnnotationState -> Int)
                  -> (Map.Map a Int -> Int -> AnnotationIO ())
                  -> AnnotationIO Int
updateOrReadIndex elem mapToGet indexToGet mapUpdate = do
  map <- gets mapToGet
  nextIndex <- gets indexToGet
  case Map.insertLookupWithKey (\_ _ -> id) elem nextIndex map of
    (Nothing, newMap) -> do
      mapUpdate newMap nextIndex
      return nextIndex

    (Just currentIndex, _) ->
      return currentIndex

annotateUnary :: (IndexedFormula -> (Int -> IndexedFormula)) -> PLTL -> PLTL -> AnnotationIO (IndexedFormula, Int)
annotateUnary toIndexed formula f = do
  annotatedF <- annotateFormula f
  index <- updateOrReadIndex formula prevs prevIndex updatePrevs
  modify (\s -> s { indexedPrevs = ((toIndexed annotatedF) index, index) : indexedPrevs s })
  return ((toIndexed annotatedF) index, index)
  where
    updatePrevs newPrevs newIndex =
      modify (\s -> s { prevs = newPrevs, prevIndex = newIndex + 1 })

-- TODO: Clean up
annotateFormula :: PLTL -> AnnotationIO IndexedFormula
annotateFormula formula =
  case formula of
    PLTL.T -> return T
    PLTL.F -> return F
    PLTL.Prop _ p ->
      liftM Prop $ updateOrReadIndex p vars varIndex updateVars
    PLTL.BinOp PLTL.Or f g ->
      liftM2 Or (annotateFormula f) (annotateFormula g)
    PLTL.BinOp PLTL.Xor f g ->
      liftM2 Xor (annotateFormula f) (annotateFormula g)
    PLTL.BinOp PLTL.And f g ->
      liftM2 And (annotateFormula f) (annotateFormula g)
    PLTL.UnOp PLTL.Not f ->
      liftM Not (annotateFormula f)
    PLTL.UnOp PLTL.Prev f ->
      liftM (uncurry Prev) $ annotateUnary const formula f
    PLTL.UnOp PLTL.WPrev f -> do
      annotatedF <- annotateFormula f
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = ((Not annotatedF), index) : indexedPrevs s })
      return (WPrev annotatedF index)
    PLTL.UnOp PLTL.Once f ->
      liftM fst $ annotateUnary Once (PLTL.UnOp PLTL.Prev formula) f
    PLTL.UnOp PLTL.Hist f -> do
      annotatedF <- annotateFormula f
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = (Not (Histor annotatedF index), index) : indexedPrevs s })
      return (Histor annotatedF index)
    PLTL.BinOp PLTL.Since f g -> do
      annotatedF <- annotateFormula f
      annotatedG <- annotateFormula g
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = (Since annotatedF annotatedG index, index) : indexedPrevs s })
      return (Since annotatedF annotatedG index)
    PLTL.BinOp PLTL.SinceW f g -> do
      annotatedF <- annotateFormula f
      annotatedG <- annotateFormula g
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = (Not (SinceW annotatedF annotatedG index), index) : indexedPrevs s })
      return (SinceW annotatedF annotatedG index)
    PLTL.BinOp PLTL.Impl f g ->
      liftM2 Impl (annotateFormula f) (annotateFormula g)
    PLTL.BinOp PLTL.Iff f g ->
      liftM2 Iff (annotateFormula f) (annotateFormula g)
    _ -> liftIO $ die $ "ppLTLTT: Cannot handle future fragment of LTL: " ++ PLTL.pltlToStringInfix formula
    where updateVars newVars newIndex =
            modify (\s -> s { vars = newVars, varIndex = newIndex + 1 })
          updatePrevs newPrevs newIndex =
            modify (\s -> s { prevs = newPrevs, prevIndex = newIndex + 1 })

---- Entry point ----

getHandle :: Maybe String -> IO Handle
getHandle Nothing = return stdout
getHandle (Just filename) = openFile filename WriteMode

processSpec :: Handle -> String -> PLTL -> IO ()
processSpec handle monitorVariable f = do
  (indexedFormula, annotationState) <- runStateT (annotateFormula f) annotationInitState
  let prevSubformulae = indexedPrevs annotationState
      variables = sortBy (\x y -> compare (snd x) (snd y)) $ Map.toList $ vars annotationState
  s <- execStateT
         (processState (0, 0))
         (initPastState indexedFormula prevSubformulae (fmap (\(p, i) -> (show (i + 1), i)) variables))
  let bodyOutput = reverse (output s)
  hPutStr handle $ unlines $ makeHeader monitorVariable (fmap fst variables)
  hPutStrLn handle "--BODY--"
  hPutStr handle $ unlines bodyOutput
  hPutStrLn handle "--END--"
  unless (handle == stdout) $ hClose handle

makeHeader :: String -> [String] -> [String]
makeHeader monitorVariable variables =
  [ "HOA: v1"
  , "Start: 0"
  , "acc-name: all"
  , "Acceptance: 1 t"
  , "AP: " ++ show (length variables + 1) ++ " " ++ variableList
  ]
  where variableList = concat $ intersperse " " $ fmap show (monitorVariable : variables)

main :: IO ()
main = do
  args <- try Args.parseArgs :: IO (Either ExitCode Args.PPLTLTTArgs)
  case args of
    Left _ ->
      return ()
    Right parsedArgs -> do
      handle <- getHandle (Args.outputFile parsedArgs)
      case (Args.inputType parsedArgs) of
        Direct str   -> processInput handle (Args.monitorVariable parsedArgs) str
        FromFile str -> readFile str >>= processInput handle (Args.monitorVariable parsedArgs)

processInput :: Handle -> String -> String -> IO ()
processInput handle monVar str =
  case parseSpot str of
    Left err   -> liftIO $ die err
    Right pltl -> processSpec handle monVar pltl

