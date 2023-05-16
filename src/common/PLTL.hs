{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module PLTL where

import Data.List ( intercalate )

data PLTL = Sl Slugs
          | UnOp UnOp PLTL
          | BinOp BinOp PLTL PLTL
          | Prop Quoted String
          | T
          | F
  deriving (Eq, Ord, Show)

data Quoted = IsQuoted | NotQuoted
  deriving (Eq, Ord, Show)

data Slugs = Section String
           | Memory Int [PLTL]
           | Recall Int
           | Comment String
           | EmptyLine
  deriving (Eq, Ord, Show)

data UnOp = Prev
          | WPrev
          | Next
          | Glob
          | Hist
          | Fut
          | Not
          | Once
  deriving (Eq, Ord, Show)


data BinOp = Since
           | WSince
           | Until
           | WUntil
           | Release
           | SRelease
           | And
           | Or
           | Xor
           | Impl
           | Iff
  deriving (Eq, Ord, Show)

data OperatorType = Boolean | Future | Past | Slugs
  deriving (Eq)

operatorType :: PLTL -> OperatorType
operatorType pltl =
  case pltl of
    Sl _               -> Slugs
    UnOp Prev _        -> Past
    UnOp WPrev _       -> Past
    UnOp Next _        -> Future
    UnOp Glob _        -> Future
    UnOp Hist _        -> Past
    UnOp Fut _         -> Future
    UnOp Not _         -> Boolean
    UnOp Once _        -> Past
    BinOp Since _ _    -> Past
    BinOp WSince _ _   -> Past
    BinOp Until _ _    -> Future
    BinOp WUntil _ _   -> Future
    BinOp Release _ _  -> Future
    BinOp SRelease _ _ -> Future
    BinOp And _ _      -> Boolean
    BinOp Or _ _       -> Boolean
    BinOp Xor _ _      -> Boolean
    BinOp Impl _ _     -> Boolean
    BinOp Iff _ _      -> Boolean
    Prop _ _           -> Boolean
    T                  -> Boolean
    F                  -> Boolean

isPPLTL :: PLTL -> Bool
isPPLTL pltl =
  case pltl of
    Sl _              -> False
    UnOp op e         -> (operatorType pltl == Past || operatorType pltl == Boolean) && isPPLTL e
    BinOp op e1 e2    -> (operatorType pltl == Past || operatorType pltl == Boolean) && isPPLTL e1 && isPPLTL e2
    Prop _ _          -> True
    T                 -> True
    F                 -> True

isPPLTLNoBool :: PLTL -> Bool
isPPLTLNoBool pltl = isPPLTL pltl && operatorType pltl == Past

containsPPLTLOperator :: PLTL -> Bool
containsPPLTLOperator pltl =
  case pltl of
    Sl _              -> False
    UnOp op e         -> operatorType pltl == Past || containsPPLTLOperator e
    BinOp op e1 e2    -> operatorType pltl == Past || containsPPLTLOperator e1 || containsPPLTLOperator e2
    Prop _  _         -> False
    T                 -> False
    F                 -> False

withParens :: String -> String
withParens s = "(" ++ s ++ ")"

pltlToStringInfix :: PLTL -> String
pltlToStringInfix (Sl (Section "")) = ""
pltlToStringInfix (Sl (Section sec)) = "[" ++ sec ++ "]"
pltlToStringInfix (Sl (Memory i es)) = "$ " ++ show i ++ " " ++ intercalate " " (fmap (withParens . pltlToStringInfix) es)
pltlToStringInfix (Sl (Recall i)) = "? " ++ show i
pltlToStringInfix (Sl (Comment str)) = "# " ++ str
pltlToStringInfix (Sl EmptyLine) = ""
pltlToStringInfix (UnOp Not (UnOp Not e1)) = pltlToStringInfix e1
pltlToStringInfix (UnOp op e) = unopToString op ++ " " ++ withParens (pltlToStringInfix e)
pltlToStringInfix (BinOp op e1 e2) = withParens (pltlToStringInfix e1) ++ " " ++ binopToString op ++ " " ++ withParens (pltlToStringInfix e2)
pltlToStringInfix (Prop IsQuoted p) = "\"" ++ p ++ "\""
pltlToStringInfix (Prop NotQuoted p) = p
pltlToStringInfix T = "1"
pltlToStringInfix F = "0"

pltlToStringPrefix :: PLTL -> String
pltlToStringPrefix (Sl (Section "")) = ""
pltlToStringPrefix (Sl (Section sec)) = "[" ++ sec ++ "]"
pltlToStringPrefix (Sl (Memory i es)) = "$ " ++ show i ++ " " ++ intercalate " " (fmap pltlToStringPrefix es)
pltlToStringPrefix (Sl (Recall i)) = "? " ++ show i
pltlToStringPrefix (Sl (Comment str)) = "#" ++ str
pltlToStringPrefix (Sl EmptyLine) = ""
pltlToStringPrefix (UnOp Next (UnOp Not e)) = "! " ++ pltlToStringPrefix (UnOp Next e)
pltlToStringPrefix (UnOp Next (Prop q p)) = pltlToStringPrefix (Prop q p) ++ "'"
pltlToStringPrefix (UnOp Not (UnOp Not e1)) = pltlToStringPrefix e1
pltlToStringPrefix (UnOp op e) = unopToString op ++ " " ++ pltlToStringPrefix e
pltlToStringPrefix (BinOp Impl e1 e2) = "| " ++ pltlToStringPrefix (UnOp Not e1) ++ " " ++ pltlToStringPrefix e2
pltlToStringPrefix (BinOp op e1 e2) = binopToString op ++ " " ++ pltlToStringPrefix e1 ++ " " ++ pltlToStringPrefix e2
pltlToStringPrefix (Prop NotQuoted p) = p
pltlToStringPrefix (Prop IsQuoted p) = "\"" ++ p ++ "\""
pltlToStringPrefix T = "1"
pltlToStringPrefix F = "0"

unopToString :: UnOp -> String
unopToString Prev = "Y"
unopToString WPrev = "T"
unopToString Next = "X"
unopToString Glob = "G"
unopToString Hist = "H"
unopToString Fut  = "F"
unopToString Not  = "!"
unopToString Once = "O"

binopToString :: BinOp -> String
binopToString Since    = "S"
binopToString WSince   = "Z"
binopToString Until    = "U"
binopToString WUntil   = "W"
binopToString Release  = "R"
binopToString SRelease = "M"
binopToString And      = "&"
binopToString Or       = "|"
binopToString Xor      = "^"
binopToString Impl     = "->"
binopToString Iff      = "<->"
