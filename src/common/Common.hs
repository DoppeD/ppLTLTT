{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module Common where

import Control.Monad.Identity         ( Identity )
import Control.Monad.Trans.State.Lazy ( StateT, execStateT, modify )
import Data.Char                      ( isDigit )
import Data.List                      ( intercalate )
import Data.Functor.Identity          ( runIdentity )
import System.Exit                    ( ExitCode(..), exitFailure )
import System.Process                 ( readProcessWithExitCode )

import PLTL

import qualified Data.Set as Set

busyIndices :: PLTL -> Set.Set Int
busyIndices pltl = runIdentity $ execStateT (findBusyIndices pltl) Set.empty

mapBusyIndices :: [PLTL] -> Set.Set Int
mapBusyIndices pltls = runIdentity $ execStateT (mapM_ findBusyIndices pltls) Set.empty

findBusyIndices :: PLTL -> StateT (Set.Set Int) Identity ()
findBusyIndices (Sl (Memory _ es)) = mapM_ findBusyIndices es
findBusyIndices (UnOp op e1)       = findBusyIndices e1
findBusyIndices (BinOp op e1 e2)   = findBusyIndices e1 >> findBusyIndices e2
findBusyIndices (Prop _ p)         = checkBusyIndex p
findBusyIndices _                  = return ()

checkBusyIndex :: String -> StateT (Set.Set Int) Identity ()
checkBusyIndex ('z' : rest) =
  case (takeWhile isDigit rest) of
    [] ->
      return ()
    digits ->
      modify (Set.insert (read digits))
checkBusyIndex _ = return ()

data AbstractionPreference = BreakAtBoolean | IncludeBoolean
  deriving (Show)

data StateEncoding = Binary | OneHot
  deriving (Show)

data InputType = Direct String | FromFile String

data UnknownArg = Argument String | Option String

argToString :: UnknownArg -> String
argToString (Argument str) = str
argToString (Option str)   = str

tryReadProcess :: String -> String -> IO () -> String -> [String] -> String -> IO String
tryReadProcess callerName calleeName onError path args stdin = do
  callResult <- readProcessWithExitCode path args stdin
  case callResult of
    (ExitSuccess, stdout, stderr) ->
      -- SLUGS writes to stderr for some reason
      return (stderr ++ stdout)
    (ExitFailure i, _, stderr) -> do
      putStrLn stderr
      putStrLn ""
      putStrLn $ callerName ++ ": " ++ calleeName ++ " exited with error code " ++ show i ++ "."
      putStrLn $ calleeName ++ " was called with the following arguments:"
      putStrLn $ intercalate " " args
      putStrLn "See stderr output above."
      onError
      exitFailure

tryReadProcessNoAct :: String -> String -> String -> [String] -> String -> IO String
tryReadProcessNoAct callerName calleeName = tryReadProcess callerName calleeName (return ())
