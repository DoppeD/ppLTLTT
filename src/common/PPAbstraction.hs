{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module PPAbstraction ( performAbstraction ) where

import Common ( AbstractionPreference(..) )
import PLTL

import Control.Monad       ( liftM, liftM2 )
import Control.Monad.State ( State, get, modify, runState )

import qualified Data.Map as Map
import qualified Data.Set as Set

performAbstraction :: AbstractionPreference -> Set.Set Int -> [(String, PLTL)] -> ([(String, PLTL)], Map.Map PLTL Int)
performAbstraction absPref busyIndices pltls =
  (newPLTLS, toConvert finalState)
  where (newPLTLS, finalState) =
          runState (mapM (performAbstraction' absPref) pltls) $ initPPAbsState busyIndices

performAbstraction' :: AbstractionPreference -> (String, PLTL) -> State PPAbsState (String, PLTL)
performAbstraction' absPref (section, pltl) = do
  modify (\s -> s { section = section })
  newPLTL <- abstractionFunction pltl
  return (section, newPLTL)
  where abstractionFunction =
          case absPref of
            BreakAtBoolean -> processPLTLBreakAtBoolean
            IncludeBoolean -> processPLTLIncludeBoolean

data PPAbsState = PPAbsState
  { busyIndices  :: Set.Set Int
  , nextIndex    :: Int
  , section      :: String
  , toConvert    :: Map.Map PLTL Int
  }

initPPAbsState :: Set.Set Int -> PPAbsState
initPPAbsState busyIndices = PPAbsState
  { busyIndices = busyIndices
  , nextIndex   = 1
  , section     = ""
  , toConvert   = Map.empty
  }

processPLTLBreakAtBoolean :: PLTL -> State PPAbsState PLTL
processPLTLBreakAtBoolean pltl =
  if (isPPLTLNoBool pltl) then
    abstractPPLTL pltl
  else
    case pltl of
      UnOp op e      -> liftM (UnOp op) $ processPLTLBreakAtBoolean e
      BinOp op e1 e2 -> liftM2 (BinOp op) (processPLTLBreakAtBoolean e1) $ processPLTLBreakAtBoolean e2
      _              -> return pltl

processPLTLIncludeBoolean :: PLTL -> State PPAbsState PLTL
processPLTLIncludeBoolean pltl =
  if (isPPLTL pltl && containsPPLTLOperator pltl) then
    abstractPPLTL pltl
  else
    case pltl of
      UnOp op e      -> liftM (UnOp op) $ processPLTLIncludeBoolean e
      BinOp op e1 e2 -> liftM2 (BinOp op) (processPLTLIncludeBoolean e1) $ processPLTLIncludeBoolean e2
      _              -> return pltl

-- Replace PPLTL subformulae with next free variable
abstractPPLTL :: PLTL -> State PPAbsState PLTL
abstractPPLTL ppltl = do
  PPAbsState { nextIndex, section, toConvert } <- get
  newIndex <- getNewIndex
  case Map.insertLookupWithKey (\_ _ -> id) ppltl newIndex toConvert of
    (Nothing, newToConvert) -> do
      modify (\s -> s { toConvert = newToConvert , nextIndex = newIndex + 1 })
      return $ Prop NotQuoted $ "z" ++ show newIndex
    (Just index, _)        ->
      return $ Prop NotQuoted $ "z" ++ show index

getNewIndex :: State PPAbsState Int
getNewIndex = do
  PPAbsState { busyIndices, nextIndex } <- get
  if (Set.member nextIndex busyIndices) then do
    modify (\s -> s { nextIndex = nextIndex + 1 })
    getNewIndex
  else
    return nextIndex
