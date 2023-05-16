{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ParserStrix ( parseStrix ) where

import Control.Monad          ( void )
import Control.Monad.Identity ( Identity )
import Data.Char              ( isDigit )

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String  ( Parser )

import PLTL ( PLTL )
import qualified PLTL

parseStrix :: String -> Either String PLTL
parseStrix str =
  case parseWithWhitespace parseStrix' str of
    Left err   -> Left $ show err
    Right pltl -> Right pltl

parseStrix' :: Parser PLTL
parseStrix' = buildExpressionParser spotTable term

term :: Parser PLTL
term = parentheses <|> try constant <|> literal

spotTable :: OperatorTable String () Identity PLTL
spotTable = [ [ prefix unary ]
            , [ Infix binaryWithoutAndOr AssocRight ]
            , [ Infix opAnd AssocLeft ]
            , [ Infix opOr AssocLeft ]
            ]
  where prefix p = Prefix . chainl1 p $ return (.)

parentheses :: Parser PLTL
parentheses = do
  void $ lexeme $ char '('
  exp <- lexeme $ parseStrix'
  void $ lexeme $ char ')'
  return exp

-- Atomic propositions --

literal :: Parser PLTL
literal = quotedLiteral <|> unquotedLiteral

quotedLiteral :: Parser PLTL
quotedLiteral = do
  void $ lexeme $ char '\"'
  p <- lexeme $ many1 $ satisfy ((/=) '"')
  void $ lexeme $ char '\"'
  return (PLTL.Prop PLTL.IsQuoted p)

unquotedLiteral :: Parser PLTL
unquotedLiteral = do
  p <- lexeme $ many1 alphaNum
  if ((isReserved p) || isDigit (head p)) then
    fail $ "Can't be used as a proposition: '" ++ p ++ "."
  else
    return (PLTL.Prop PLTL.NotQuoted p)

-- Reserved strings --

isReserved :: String -> Bool
isReserved = (flip elem) reservedOps
  where reservedOps =
          [ "xor", "XOR", "true", "false"
          , "NOT", "IMP", "BIIMP", "AND", "OR"
          , "F", "G", "U", "M", "R", "W", "X"
          , "O", "H", "S", "Z", "Y"
          ]

-- Constants --

constant :: Parser PLTL
constant = true <|> false

true :: Parser PLTL
true = do
  lexeme $
    choice
      [ void (char '1')
      , try (void (mapM_ (\a -> lexeme (char a)) "true"))
      , void (mapM_ (\a -> lexeme (char a)) "tt")
      ]
  return PLTL.T

false :: Parser PLTL
false = do
  lexeme $
    choice
      [ void (char '0')
      , try (void (mapM_ (\a -> lexeme (char a)) "false"))
      , void (mapM_ (\a -> lexeme (char a)) "ff")
      ]
  return PLTL.F

-- Unary operators --

unary :: Parser (PLTL -> PLTL)
unary =
  choice
    [ opNot
    , opHist
    , opOnce
    , opPrev
    , opWPrev
    , opNext
    , opGlob
    , opFut
    ]

opNot :: Parser (PLTL -> PLTL)
opNot = do
  strings ["!", "NOT"]
  return (PLTL.UnOp PLTL.Not)

opHist :: Parser (PLTL -> PLTL)
opHist = do
  strings ["H"]
  return (PLTL.UnOp PLTL.Hist)

opOnce :: Parser (PLTL -> PLTL)
opOnce = do
  strings ["O"]
  return (PLTL.UnOp PLTL.Once)

opPrev :: Parser (PLTL -> PLTL)
opPrev = do
  strings ["Y"]
  return (PLTL.UnOp PLTL.Prev)

opWPrev :: Parser (PLTL -> PLTL)
opWPrev = do
  strings ["T"]
  return (PLTL.UnOp PLTL.WPrev)

opNext :: Parser (PLTL -> PLTL)
opNext = do
  strings ["X"]
  return (PLTL.UnOp PLTL.Next)

opGlob :: Parser (PLTL -> PLTL)
opGlob = do
  strings ["G"]
  return (PLTL.UnOp PLTL.Glob)

opFut :: Parser (PLTL -> PLTL)
opFut = do
  strings ["F"]
  return (PLTL.UnOp PLTL.Fut)

-- Binary operators --

binaryWithoutAndOr :: Parser (PLTL -> PLTL -> PLTL)
binaryWithoutAndOr =
  choice
    [ opImpl
    , opIff
    , opXor
    , opUntil
    , opWUntil
    , opRelease
    , opSRelease
    , opSince
    , opWSince
    ]

opAnd :: Parser (PLTL -> PLTL -> PLTL)
opAnd = do
  strings ["&&", "&", "AND"]
  return (PLTL.BinOp PLTL.And)

opOr :: Parser (PLTL -> PLTL -> PLTL)
opOr = do
  strings ["||", "|", "OR"]
  return (PLTL.BinOp PLTL.Or)

opImpl :: Parser (PLTL -> PLTL -> PLTL)
opImpl = do
  strings ["->", "=>", "IMP"]
  return (PLTL.BinOp PLTL.Impl)

opIff :: Parser (PLTL -> PLTL -> PLTL)
opIff = do
  strings ["<->", "<=>", "BIIMP"]
  return (PLTL.BinOp PLTL.Iff)

opXor :: Parser (PLTL -> PLTL -> PLTL)
opXor = do
  strings ["^", "xor", "XOR"]
  return (PLTL.BinOp PLTL.Xor)

opUntil :: Parser (PLTL -> PLTL -> PLTL)
opUntil = do
  strings ["U"]
  return (PLTL.BinOp PLTL.Until)

opWUntil :: Parser (PLTL -> PLTL -> PLTL)
opWUntil = do
  strings ["W"]
  return (PLTL.BinOp PLTL.WUntil)

opRelease :: Parser (PLTL -> PLTL -> PLTL)
opRelease = do
  strings ["R"]
  return (PLTL.BinOp PLTL.Release)

opSRelease :: Parser (PLTL -> PLTL -> PLTL)
opSRelease = do
  strings ["M"]
  return (PLTL.BinOp PLTL.Release)

opSince :: Parser (PLTL -> PLTL -> PLTL)
opSince = do
  strings ["S"]
  return (PLTL.BinOp PLTL.Since)

opWSince :: Parser (PLTL -> PLTL -> PLTL)
opWSince = do
  strings ["Z"]
  return (PLTL.BinOp PLTL.WSince)

-- Helper functions --

strings :: [String] -> Parser ()
strings = void . choice . fmap (try . lexeme . string)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where wrapper = whitespace >> p

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\n"

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
