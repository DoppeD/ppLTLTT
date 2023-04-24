{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module Main ( main ) where

import Control.Exception              ( try )
import Control.Monad                  ( foldM, liftM, void )
import Control.Monad.IO.Class         ( liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, execStateT, get, gets, modify )
import Data.List                      ( partition, sortOn )
import Data.Maybe                     ( fromMaybe, listToMaybe )
import System.Directory               ( removeFile )
import System.Exit                    ( ExitCode(..), die )
import System.IO                      ( hClose, hPutStr, openTempFile )

import qualified ArgsPSlugs as Args

import Common
import EncodeTT
import ParserHOA
import ParserSlugs
import PLTL
import PPAbstraction

import qualified Data.Map as Map

main :: IO ()
main = do
  args <- try (Args.parseArgs >>= makeState) :: IO (Either ExitCode PSlugsState)
  case args of
    Left _ ->
      return ()
    Right initState ->
      void $ execStateT (processFile parseSlugsin (inputFile initState)) initState

data PSlugsState = PSlugsState
  { abstractionPreference :: AbstractionPreference
  , currentSection        :: String
  , inputFile             :: String
  , outputFile            :: String
  , sectionOrder          :: Map.Map String Int
  , slugsArguments        :: [String]
  , stateEncoding         :: StateEncoding
  }

makeState :: Args.PSlugsArgs -> IO PSlugsState
makeState args =
  if (length files == 0) then
    die "pSLUGS: No files to process!"
  else
    return
      PSlugsState
        { abstractionPreference = Args.abstractionPreference args
        , currentSection        = ""
        , inputFile             = infile
        , outputFile            = outfile
        , sectionOrder          = Map.empty
        , slugsArguments        = options
        , stateEncoding         = Args.stateEncoding args
        }
  where (files, options) = mapTuple (fmap argToString) $ partition notOption (Args.unknownArguments args)
        notOption (Option _) = False
        notOption _ = True
        infile = head files
        outfile = fromMaybe "" $ listToMaybe $ drop 1 files

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

processFile :: (String -> Either String [PLTL]) -> String -> StateT PSlugsState IO ()
processFile parser file = do
  contents <- liftIO $ readFile file
  case parser contents of
    Left err      -> liftIO $ die err
    Right slugsin -> processSpec slugsin

processSpec :: [PLTL] -> StateT PSlugsState IO ()
processSpec pltls = do
  PSlugsState { abstractionPreference, outputFile, slugsArguments } <- get
  pltls' <- mapM processPLTL pltls
  let (newPltls, ppltlToConvert) = performAbstraction abstractionPreference (mapBusyIndices pltls) pltls'
  encodedTTs <- foldM (\acc pltl -> liftM ((++) acc) (processPPLTL pltl)) [] (Map.toList ppltlToConvert)
  newSpec <- orderSections $ Map.toList $ Map.fromListWith (++) $ fmap singletonSnd $ newPltls ++ encodedTTs
  liftIO $ sendSpecToSlugs slugsArguments outputFile newSpec

singletonSnd :: (a, b) -> (a, [b])
singletonSnd = fmap (\x -> [x])

processPLTL :: PLTL -> StateT PSlugsState IO (String, PLTL)
processPLTL (Sl (Section sec)) = do
  modify (\s -> s { currentSection = sec, sectionOrder = Map.insert sec (Map.size (sectionOrder s)) (sectionOrder s) })
  return (sec, Sl (Section sec))
processPLTL pltl = do
  currentSec <- gets currentSection
  return (currentSec, pltl)

processPPLTL :: (PLTL, Int) -> StateT PSlugsState IO [(String, PLTL)]
processPPLTL (ppltl, index) = do
  PSlugsState { stateEncoding } <- get
  tt <- liftIO $ tryReadProcessNoAct "pSLUGS" "ppLTLTT" "ppLTLTT" args []
  case (parseHOA tt >>= encodeTT stateEncoding bitToString) of
    Left err    -> liftIO $ die err
    Right encTT -> return encTT
  where args = ["-m", 'z' : show index, "-f", pltlToStringInfix ppltl]

bitToString :: String -> Int -> Int -> String
bitToString var max 0   = var ++ "@0.0." ++ show max
bitToString var max bit = var ++ "@" ++ show bit

specToString :: (PLTL -> String) -> (String, [PLTL]) -> String
specToString stringifier (sec, pltl) =
  unlines $ fmap stringifier $ Sl (Section sec) : filter noSec (reverse pltl)
  where noSec (Sl (Section _)) = False
        noSec _                = True

sendSpecToSlugs :: [String] -> String -> [(String, [PLTL])] -> IO ()
sendSpecToSlugs args outputFile spec = do
  (tempFilePath, tempFileHandle) <- openTempFile "." "pslugs"
  hPutStr tempFileHandle $ unlines $ fmap (specToString pltlToStringPrefix) spec
  hClose tempFileHandle
  let allArgs = args ++ (tempFilePath : outfile)
  callResult <- tryReadProcess "pSLUGS" "SLUGS" (removeFile tempFilePath) "slugs" allArgs []
  removeFile tempFilePath
  putStrLn callResult
  where outfile = if (outputFile == "") then [] else [outputFile]

orderSections :: [(String, [PLTL])] -> StateT PSlugsState IO [(String, [PLTL])]
orderSections spec = do
  PSlugsState { sectionOrder } <- get
  let ordering ("", _) = 0
      ordering (sec, _) =
        case Map.lookup sec sectionOrder of
          Just i -> i
          _ -> 1 + Map.size sectionOrder
  return $ sortOn ordering spec
