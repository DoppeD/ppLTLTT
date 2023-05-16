{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module Main where

import qualified Data.Map as Map

import Control.Exception      ( try )
import Control.Monad          ( liftM, liftM2, liftM3, unless )
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
    | UnOp UnOp IndexedFormula Int
    | BinOp BinOp IndexedFormula IndexedFormula Int
    | T
    | F
  deriving (Eq, Ord, Show, Read)

data UnOp
    = Not
    | Prev
    | WPrev
    | Once
    | Hist
  deriving (Eq, Ord, Show, Read)

data BinOp
    = Since
    | WSince
    | And
    | Or
    | Xor
    | Impl
    | Iff
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
eval state val (BinOp Or f g _) = (eval state val f) || (eval state val g)
eval state val (BinOp Xor f g _) = (eval state val f) /= (eval state val g)
eval state val (BinOp And f g _) = (eval state val f) && (eval state val g)
eval state val (UnOp Not f _) = not (eval state val f)
eval state val (UnOp Prev _ i) = testBit state i
eval state val (UnOp WPrev _ i) = not (testBit state i)
eval state val (UnOp Once f i) = (testBit state i) || (eval state val f)
eval state val (UnOp Hist f i) = (not (testBit state i)) && (eval state val f)
eval state val (BinOp Since f g i) =
  ((testBit state i) && (eval state val f)) || (eval state val g)
eval state val (BinOp WSince f g i) =
  (not (testBit state i) && (eval state val f)) || (eval state val g)
eval state val (BinOp Impl f g _) = (not (eval state val f)) || (eval state val g)
eval state val (BinOp Iff f g _) = (eval state val f) == (eval state val g)
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
      liftM3 (BinOp Or) (annotateFormula f) (annotateFormula g) (return 0)
    PLTL.BinOp PLTL.Xor f g ->
      liftM3 (BinOp Xor) (annotateFormula f) (annotateFormula g) (return 0)
    PLTL.BinOp PLTL.And f g ->
      liftM3 (BinOp And) (annotateFormula f) (annotateFormula g) (return 0)
    PLTL.UnOp PLTL.Not f ->
      liftM2 (UnOp Not) (annotateFormula f) (return 0)
    PLTL.UnOp PLTL.Prev f ->
      liftM (uncurry (UnOp Prev)) $ annotateUnary const formula f
    PLTL.UnOp PLTL.WPrev f -> do
      annotatedF <- annotateFormula f
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = ((UnOp Not annotatedF 0), index) : indexedPrevs s })
      return (UnOp WPrev annotatedF index)
    PLTL.UnOp PLTL.Once f ->
      liftM fst $ annotateUnary (UnOp Once) (PLTL.UnOp PLTL.Prev formula) f
    PLTL.UnOp PLTL.Hist f -> do
      annotatedF <- annotateFormula f
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = (UnOp Not (UnOp Hist annotatedF index) 0, index) : indexedPrevs s })
      return (UnOp Hist annotatedF index)
    PLTL.BinOp PLTL.Since f g -> do
      annotatedF <- annotateFormula f
      annotatedG <- annotateFormula g
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = (BinOp Since annotatedF annotatedG index, index) : indexedPrevs s })
      return (BinOp Since annotatedF annotatedG index)
    PLTL.BinOp PLTL.WSince f g -> do
      annotatedF <- annotateFormula f
      annotatedG <- annotateFormula g
      index <- updateOrReadIndex formula prevs prevIndex updatePrevs
      modify (\s -> s { indexedPrevs = (UnOp Not (BinOp WSince annotatedF annotatedG index) 0, index) : indexedPrevs s })
      return (BinOp WSince annotatedF annotatedG index)
    PLTL.BinOp PLTL.Impl f g ->
      liftM3 (BinOp Impl) (annotateFormula f) (annotateFormula g) (return 0)
    PLTL.BinOp PLTL.Iff f g ->
      liftM3 (BinOp Iff) (annotateFormula f) (annotateFormula g) (return 0)
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

