{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module EncodeTT ( encodeTT ) where

import Common ( StateEncoding(..) )
import HOA
import PLTL

import Control.Monad             ( liftM )
import Control.Monad.Except      ( throwError )
import Control.Monad.Trans.State ( StateT, execStateT, get, gets, modify )
import Data.Bits                 ( testBit )
import Data.Maybe                ( catMaybes )

data EncodeTTState = EncodeTTState
  { bitToString   :: BitToString
  , currentState  :: Int
  , encoding      :: [(String, PLTL)]
  , monitorVar    :: String
  , numStates     :: Int
  , startState    :: Int
  , stateEncoding :: StateEncoding
  , vars          :: [String]
  }

initEncodeTTState :: StateEncoding -> BitToString -> EncodeTTState
initEncodeTTState stateEncoding bitToString = EncodeTTState
  { bitToString   = bitToString
  , currentState  = 0
  , encoding      = []
  , monitorVar    = []
  , numStates     = 0
  , startState    = 0
  , stateEncoding = stateEncoding
  , vars          = []
  }

type BitToString = String -> Int -> Int -> String
type EncTTErr = StateT EncodeTTState (Either String)

encodeTT :: StateEncoding -> BitToString -> [HOA] -> Either String [(String, PLTL)]
encodeTT stateEncoding bitToString hoas = liftM encoding finalState
  where finalState = execStateT (encodeHOAS hoas) $ initEncodeTTState stateEncoding bitToString

countStates :: [HOA] -> Int
countStates = foldl (\acc -> (+) acc . countStates') 0
  where countStates' (State _) = 1
        countStates' _         = 0

encodeHOAS :: [HOA] -> EncTTErr ()
encodeHOAS hoas = do
  modify (\s -> s { numStates = countStates hoas })
  mapM_ encodeHOA hoas
  startStateLTL <- gets startState >>= encodeState CurrentState
  maybe (return ()) (addToEncoding "SYS_INIT") startStateLTL
  EncodeTTState { bitToString, monitorVar, numStates, stateEncoding } <- get
  addToEncoding "OUTPUT" (Prop NotQuoted monitorVar)
  let varsNeeded = variablesNeededForState stateEncoding numStates
  if (varsNeeded == 0) then
    return ()
  else
    mapM_ (\i -> addToEncoding "OUTPUT" (Prop NotQuoted (bitToString monitorVar (numStates - 1) i))) [0..varsNeeded - 1]

encodeHOA :: HOA -> EncTTErr ()
encodeHOA hoa =
  case hoa of
    AP [] ->
      throwError "No variables in AP!"
    AP (monVar : vars) ->
      modify (\s -> s { monitorVar = monVar, vars = vars })
    StartState st ->
      modify (\s -> s { startState = st })
    State st ->
      modify (\s -> s { currentState = st })
    Trans val st -> do
      EncodeTTState { currentState } <- get
      encCurState <- encodeState CurrentState currentState
      encNxtState <- encodeState NextState st
      (encMonVal, encVal) <- encodeValuation val
      let encVal' = safeFoldl1 (BinOp And) encVal
          implicant = safeFoldl1 (BinOp And) $ catMaybes [encCurState, encVal']
          implicand = foldl1 (BinOp And) $ catMaybes [encNxtState, Just encMonVal]
          transEnc = foldl1 (BinOp Impl) $ catMaybes [implicant, Just implicand]
      addToEncoding "SYS_TRANS" transEnc
    Other -> return ()

safeFoldl1 :: (a -> a -> a) -> [a] -> Maybe a
safeFoldl1 f [] = Nothing
safeFoldl1 f x = Just $ foldl1 f x

encodeValuation :: [Valuation] -> EncTTErr (PLTL, [PLTL])
encodeValuation [] =
  throwError "Empty valuation in transition!"
encodeValuation (monVar : val) = do
  EncodeTTState { monitorVar, vars } <- get
  return (valToProp (monVar, monitorVar), fmap valToProp (zip val vars))
  where valToProp (TrueProp _, varName) = Prop NotQuoted varName
        valToProp (FalseProp _, varName) = UnOp Not (Prop NotQuoted varName)

data StateType = CurrentState | NextState

stateTypeTransform :: StateType -> PLTL -> PLTL
stateTypeTransform CurrentState = id
stateTypeTransform NextState = UnOp Next

bitTestFunction :: StateEncoding -> Int -> Int -> Bool
bitTestFunction Binary  = testBit
bitTestFunction Decimal = \_ _ -> True -- Not needed
bitTestFunction OneHot  = (==)

encodeState :: StateType -> Int -> EncTTErr (Maybe PLTL)
encodeState stType st = do
  EncodeTTState { bitToString, monitorVar, numStates, stateEncoding } <- get
  case stateEncoding of
    Decimal ->
      return $ Just $ Sl $ ArithOp EQQ stateProp (Prop NotQuoted (show st))
      where stateProp = stateTypeTransform stType $ Prop NotQuoted monitorVar
    _ ->
      encodeStateNonDecimal stType st

encodeStateNonDecimal :: StateType -> Int -> EncTTErr (Maybe PLTL)
encodeStateNonDecimal stType st = do
  EncodeTTState { bitToString, monitorVar, numStates, stateEncoding } <- get
  if (numStates < 2) then return Nothing else do
    let varsNeeded = variablesNeededForState stateEncoding numStates
        bitAsProp bit = Prop NotQuoted (bitToString monitorVar (numStates - 1) bit)
        bitAsLTL bit = if (bitTestFunction stateEncoding st bit) then (bitAsProp bit) else UnOp Not (bitAsProp bit)
    return $ Just $ foldl1 (BinOp And) (fmap ((stateTypeTransform stType) . bitAsLTL) [0..varsNeeded - 1])

variablesNeededForState :: StateEncoding -> Int -> Int
variablesNeededForState encoding max =
  if max < 2 then
    0
  else case encoding of
    Binary -> ceiling (logBase 2 (fromIntegral max))
    Decimal -> 1
    OneHot -> max

addToEncoding :: String -> PLTL -> EncTTErr ()
addToEncoding section pltl = modify (\s -> s { encoding = (section, pltl) : (encoding s) })
