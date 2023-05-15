{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module Main ( main ) where

import Control.Exception              ( try )
import Control.Monad                  ( foldM, liftM, void )
import Control.Monad.IO.Class         ( liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, execStateT, get, modify )
import Data.List                      ( intercalate )
import System.Directory               ( removeFile )
import System.Exit                    ( ExitCode, die )
import System.IO                      ( hClose, hPutStr, openTempFile )

import qualified ArgsPltl2tgba as Args

import Common
import ParserSpot
import PLTL
import PPAbstraction

import qualified Data.Map as Map

main :: IO ()
main = do
  args <- try (Args.parseArgs >>= makeState) :: IO (Either ExitCode PSpotState)
  case args of
    Left _ ->
      return ()
    Right initState ->
      case (inputType initState) of
        Direct str -> void $ execStateT (processInput str) initState
        FromFile str -> void $ execStateT (processFile str) initState

data PSpotState = PSpotState
  { abstractionPreference  :: AbstractionPreference
  , inputType              :: InputType
  , spotArguments          :: [String]
  , tempFiles              :: [String]
  }

makeState :: Args.PSpotArgs -> IO PSpotState
makeState args =
  return
    PSpotState
      { abstractionPreference  = Args.abstractionPreference args
      , inputType              = Args.inputType args
      , spotArguments          = fmap argToString $ Args.unknownArguments args
      , tempFiles              = []
      }

processFile :: String -> StateT PSpotState IO ()
processFile file = do
  contents <- liftIO $ readFile file
  processInput contents

processInput :: String -> StateT PSpotState IO ()
processInput str =
  case parseSpot str of
    Left err   -> liftIO $ die err
    Right pltl -> do
      PSpotState { abstractionPreference } <- get
      processSpec $ performAbstraction abstractionPreference (busyIndices pltl) [("ORIGINAL", pltl)]

processSpec :: ([(String, PLTL)], Map.Map PLTL Int) -> StateT PSpotState IO ()
processSpec (newSpec, ppltlToConvert) = do
  PSpotState { abstractionPreference, spotArguments } <- get
  (temporalTesters, abstractionVars) <- liftM unzip $ mapM ppLTLTT $ Map.toList ppltlToConvert
  abstractionAutomaton <- makeAbstractionAutomaton (snd (head newSpec)) >>= writeToTemp
  result <-
    case temporalTesters of
      [] ->
        return abstractionAutomaton
      (headTT:tailTT) -> do
        firstTT <- writeToTemp headTT
        bigTT <- foldM combineTT firstTT tailTT
        temp <- tryRead "autfilt" ["--file=" ++ bigTT, "--product=" ++ abstractionAutomaton] [] >>= writeToTemp
        tryRead "autfilt" ["--file=" ++ temp, "--remove-ap=" ++ aps, "--high"] [] >>= writeToTemp
        where aps = intercalate "," $ fmap ((:) 'z' . show) abstractionVars
  callResult <- tryRead "autfilt" ("-F" : (result : spotArguments)) []
  callResultSimp <- tryRead "autfilt" ["--high"] callResult
  liftIO $ putStrLn callResultSimp
  PSpotState { tempFiles } <- get
  liftIO $ cleanup tempFiles

makeAbstractionAutomaton :: PLTL -> StateT PSpotState IO String
makeAbstractionAutomaton pltl =
  tryRead "ltl2tgba" ["--high"] $ pltlToStringInfix pltl

ppLTLTT :: (PLTL, Int) -> StateT PSpotState IO (String, Int)
ppLTLTT (ppltl, index) = do
  tt <- tryRead "ppLTLTT" ppargs []
  tt' <- tryRead "autfilt" ["--high"] tt
  return (tt', index)
  where ppargs = ["-m", 'z' : show index, "-f", pltlToStringInfix ppltl]

combineTT :: String -> String -> StateT PSpotState IO String
combineTT file tt = do
  newTT <- tryRead "autfilt" ["--product=" ++ file, "--high"] tt
  writeToTemp newTT

writeToTemp :: String -> StateT PSpotState IO String
writeToTemp output = do
  tempFilePath <- liftIO $ do
    (tempFilePath, tempFileHandle) <- openTempFile "." "pspot"
    hPutStr tempFileHandle output
    hClose tempFileHandle
    return tempFilePath
  modify (\s -> s { tempFiles = tempFilePath : tempFiles s })
  return tempFilePath

cleanup :: [String] -> IO ()
cleanup = mapM_ removeFile

tryRead :: String -> [String] -> String -> StateT PSpotState IO String
tryRead procname args stdin = do
  PSpotState { tempFiles } <- get
  liftIO $ tryReadProcess "pltl2tgba" procname (cleanup tempFiles) procname args stdin
