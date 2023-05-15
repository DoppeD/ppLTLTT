{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ParserSlugs ( parseSlugsin ) where

import Control.Monad          ( void )
import Data.Either            ( partitionEithers )
import Data.Maybe             ( listToMaybe )

import Text.Parsec
import Text.Parsec.String ( Parser )

import PLTL ( PLTL )
import qualified PLTL

parentheses :: Parser PLTL
parentheses = do
  lexeme $ char '('
  exp <- lexeme $ parseSlugsinLine
  lexeme $ char ')'
  return exp

parseSlugsin :: String -> Either String [PLTL]
parseSlugsin str =
  case partitionEithers parsedLines of
    ([], pltl) -> Right pltl
    (errs, _)  -> Left $ concat $ fmap show errs
  where parsedLines = fmap (parseWithWhitespace (lexeme parseSlugsinLine)) (lines str)

parseSlugsinLine :: Parser PLTL
parseSlugsinLine = comment <|> section <|> expr <|> emptyline

expr :: Parser PLTL
expr = parentheses <|> memory <|> recall <|> slugsinBinary <|> slugsinUnary <|> slugsinConstant <|> literal

slugsinBinary :: Parser PLTL
slugsinBinary = do
  op <- binary
  exp1 <- lexeme $ expr
  exp2 <- lexeme $ expr
  return $ op exp1 exp2

slugsinUnary :: Parser PLTL
slugsinUnary = do
  op <- unary
  exp <- lexeme $ expr
  return $ op exp

-- Atomic propositions --

literal :: Parser PLTL
literal = do
  str <- lexeme $ many1 $ noneOf " \t:()"
  if maybe False ((==) '\'') (listToMaybe (reverse str)) then
    return $ PLTL.UnOp PLTL.Next $ PLTL.Prop PLTL.NotQuoted $ take (length str - 1) str
  else
    return $ PLTL.Prop PLTL.NotQuoted str

-- Slugs specific --

comment :: Parser PLTL
comment = do
  char '#'
  str <- many $ satisfy ((/=) '\n')
  return $ PLTL.Sl (PLTL.Comment str)

section :: Parser PLTL
section = do
  lexeme $ char '['
  str <- many $ satisfy ((/=) ']')
  lexeme $ char ']'
  return $ PLTL.Sl (PLTL.Section str)

memory :: Parser PLTL
memory = do
  void $ lexeme $ char '$'
  numExp <- lexeme $ many1 digit
  exps <- count (read numExp) (lexeme expr)
  return $ PLTL.Sl (PLTL.Memory (read numExp) exps)

recall :: Parser PLTL
recall = do
  void $ lexeme $ char '?'
  index <- lexeme $ many1 digit
  return $ PLTL.Sl (PLTL.Recall (read index))

emptyline :: Parser PLTL
emptyline = do
  many $ oneOf " \t"
  return $ PLTL.Sl PLTL.EmptyLine

-- Constants --

slugsinConstant :: Parser PLTL
slugsinConstant =
  (lexeme (char '1') >> return PLTL.T) <|> (lexeme (char '0') >> return PLTL.F)

-- Unary operators --

unary :: Parser (PLTL -> PLTL)
unary =
  choice
    [ opNot
    , opHist
    , opOnce
    , opPrev
    , opWPrev
    ]

opNot :: Parser (PLTL -> PLTL)
opNot = do
  strings ["!"]
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

-- Binary operators --

binary :: Parser (PLTL -> PLTL -> PLTL)
binary =
  choice
    [ opSince
    , opSinceW
    , opAnd
    , opOr
    , opXor
    ]

binaryTemporal :: Parser (PLTL -> PLTL -> PLTL)
binaryTemporal =
  choice
    [ opSince
    , opSinceW
    ]

opAnd :: Parser (PLTL -> PLTL -> PLTL)
opAnd = do
  strings ["&"]
  return (PLTL.BinOp PLTL.And)

opOr :: Parser (PLTL -> PLTL -> PLTL)
opOr = do
  strings ["|"]
  return (PLTL.BinOp PLTL.Or)

opXor :: Parser (PLTL -> PLTL -> PLTL)
opXor = do
  strings ["^"]
  return (PLTL.BinOp PLTL.Xor)

opSince :: Parser (PLTL -> PLTL -> PLTL)
opSince = do
  strings ["S"]
  return (PLTL.BinOp PLTL.Since)

opSinceW :: Parser (PLTL -> PLTL -> PLTL)
opSinceW = do
  strings ["Z"]
  return (PLTL.BinOp PLTL.SinceW)

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
whitespace = void $ many $ oneOf " \t"

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
