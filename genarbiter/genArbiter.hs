module Main where

import Control.Monad      ( when )
import Data.Char          ( digitToInt, intToDigit )
import Data.List
import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Text.Read          ( readMaybe )

data ArbiterType = LTL | PLTL | PGR1

type Err = Either String

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace a b (c:cs) =
  if (a == c) then
    b : replace a b cs    
  else
    c : replace a b cs

toPair :: [Int] -> (Int, Int)
toPair [a, b] = (a, b)
toPair a = (0, 0)

buildArbiter :: ArbiterType -> Int -> IO ()
buildArbiter arbiterType n = do
  let liveness = fmap (\i -> replace 'i' (intToDigit i) "(G F ri -> G F gi)") [1..n]
      livenessGr1 = fmap (\i -> replace 'i' (intToDigit i) "G F ((!ri) Z gi)") [1..n]
      noSpuriousGrantPast = fmap (\i -> replace 'i' (intToDigit i) "G (gi -> Y (!gi S ri))") [1..n]
      noSpuriousGrantFuture = fmap (\i -> replace 'i' (intToDigit i) "!((!ri) U ((!ri) & gi)) & !(F(gi & X((!ri) U ((!ri) & gi))))") [1..n]
      pairs = fmap toPair $ filter ((==) 2 . length) $ subsequences [1..n]
      mutex = fmap (\(i, j) -> replace 'j' (intToDigit j) $ replace 'i' (intToDigit i) "G (!gi | !gj)") pairs
      outputPast = intercalate " & " (liveness ++ mutex ++ noSpuriousGrantPast)
      outputFuture = intercalate " & " (liveness ++ mutex ++ noSpuriousGrantFuture)
      outputGr1 = intercalate " & " (livenessGr1 ++ mutex ++ noSpuriousGrantPast)
  case arbiterType of
    LTL  -> putStrLn outputFuture
    PLTL -> putStrLn outputPast
    PGR1 -> putStrLn outputGr1

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fs] ->
      case readMaybe fs of
        Nothing -> exitWithError usageString
        Just numVars -> buildArbiter LTL numVars
    ["-pltl", fs] ->
      case readMaybe fs of
        Nothing -> exitWithError usageString
        Just numVars -> buildArbiter PLTL numVars
    ["-pgr1", fs] ->
      case readMaybe fs of
        Nothing -> exitWithError usageString
        Just numVars -> buildArbiter PGR1 numVars
    _ ->
      exitWithError usageString

usageString :: String
usageString = 
  unlines [ "genArbiter: Outputs arbiter specifications with or without past to standard output."
          , "Usage: genArbiter [-pltl | pgr1] #clients"
          ]

exitWithError :: String -> IO ()
exitWithError s = do
  putStrLn s
  return ()
