{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module Main ( main ) where

import Control.Exception              ( try )
import Control.Monad                  ( foldM, liftM, void )
import Control.Monad.IO.Class         ( liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, execStateT, get )
import Data.List                      ( intercalate )
import Data.Maybe                     ( catMaybes, fromMaybe, maybeToList )
import System.Directory               ( removeFile )
import System.Exit                    ( ExitCode, die )
import System.IO                      ( hClose, hPutStr, openTempFile )

import qualified ArgsPStrix as Args

import Common
import EncodeTT
import ParserHOA
import ParserStrix
import PLTL
import PPAbstraction

import qualified Data.Map as Map

main :: IO ()
main = do
  args <- try (Args.parseArgs >>= makeState) :: IO (Either ExitCode PStrixState)
  case args of
    Left _ ->
      return ()
    Right initState ->
      case (inputType initState) of
        Direct str -> void $ execStateT (processInput str) initState
        FromFile str -> void $ execStateT (processFile str) initState

data PStrixState = PStrixState
  { abstractionPreference :: AbstractionPreference
  , inputType             :: InputType
  , inputVars             :: Maybe String
  , outputVars            :: Maybe String
  , strixArguments        :: [String]
  , stateEncoding         :: StateEncoding
  }

makeState :: Args.PStrixArgs -> IO PStrixState
makeState args =
  return
    PStrixState
      { abstractionPreference = Args.abstractionPreference args
      , inputType             = Args.inputType args
      , inputVars             = fmap filterStr $ Args.inputVars args
      , outputVars            = fmap filterStr $ Args.outputVars args
      , strixArguments        = fmap argToString $ Args.unknownArguments args
      , stateEncoding         = Args.stateEncoding args
      }
  where filterStr = filter (not . (flip elem) "\" ")

processFile :: String -> StateT PStrixState IO ()
processFile file = do
  contents <- liftIO $ readFile file
  processInput contents

processInput :: String -> StateT PStrixState IO ()
processInput str =
  case parseStrix str of
    Left err   -> liftIO $ die err
    Right pltl -> processSpec pltl

processSpec :: PLTL -> StateT PStrixState IO ()
processSpec spec = do
  st <- get
  let (newSpec, ppltlToConvert) = performAbstraction (abstractionPreference st) (busyIndices spec) [("ORIGINAL", spec)]
  encodedTTs <- foldM (\acc pltl -> liftM ((++) acc) (processPPLTL pltl)) [] (Map.toList ppltlToConvert)
  let specToSend = Map.fromListWith (++) $ fmap singletonSnd $ newSpec ++ encodedTTs
  liftIO $ sendSpecToStrix st specToSend

singletonSnd :: (a, b) -> (a, [b])
singletonSnd = fmap (\x -> [x])

processPPLTL :: (PLTL, Int) -> StateT PStrixState IO [(String, PLTL)]
processPPLTL (ppltl, index) = do
  PStrixState { stateEncoding } <- get
  tt <- liftIO $ tryRead "ppLTLTT" args []
  case (parseHOA tt >>= encodeTT stateEncoding bitToString) of
    Left err    -> liftIO $ die err
    Right encTT -> return encTT
  where args = ["-m", 'z' : show index, "-f", pltlToStringInfix ppltl]

bitToString :: String -> Int -> Int -> String
bitToString var max bit = var ++ "_" ++ show bit

sendSpecToStrix :: PStrixState -> Map.Map String [PLTL] -> IO ()
sendSpecToStrix st spec = do
  let finalSpec = strixify (lup "ORIGINAL") (lup "SYS_INIT") (lup "SYS_TRANS")
      newOutputs = intercalate "," $ fmap pltlToStringInfix $ fromMaybe [] $ Map.lookup "OUTPUT" spec
      outputs = makeOutputVars (maybeToList (outputVars st)) newOutputs
      inputs = makeInputVars $ maybeToList $ inputVars st
      allArgs = addIfNotEmpty inputs $ addIfNotEmpty outputs (strixArguments st)
  (tempFilePath, tempFileHandle) <- openTempFile "." "pstrix"
  mapM_ (hPutStr tempFileHandle) finalSpec
  hClose tempFileHandle
  callResult <- tryReadProcess "pStrix" "Strix" (removeFile tempFilePath) "strix" ("-F" : (tempFilePath : allArgs)) []
  removeFile tempFilePath
  putStrLn callResult
  where lup = (flip Map.lookup) spec
        addIfNotEmpty "" strs = strs
        addIfNotEmpty str strs = str : strs

makeInputVars :: [String] -> String
makeInputVars []      = ""
makeInputVars (str:_) = "--ins=" ++ str

makeOutputVars :: [String] -> String -> String
makeOutputVars [] []         = ""
makeOutputVars [] (new:rest) = "--outs=" ++ (new:rest)
makeOutputVars (old:rest) [] = "--outs=" ++ old
makeOutputVars old new       = "--outs=" ++ head old ++ "," ++ new

strixify :: Maybe [PLTL] -> Maybe [PLTL] -> Maybe [PLTL] -> [String]
strixify morig minit mtrans =
  concat $ catMaybes $ [ fmap (fmap pltlToStringInfix) morig
                       , fmap (fmap ((++) "& " . pltlToStringInfix)) minit
                       , fmap (fmap ((++) "& " . pltlToStringInfix . UnOp Glob)) mtrans
                       ]

safeFoldl1 :: (a -> a -> a) -> [a] -> Maybe a
safeFoldl1 f [] = Nothing
safeFoldl1 f x = Just $ foldl1 f x

tryRead :: String -> [String] -> String -> IO String
tryRead procname = tryReadProcessNoAct "pstrix" procname procname
