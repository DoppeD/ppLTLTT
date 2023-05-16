{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ParserSpot ( parseSpot ) where

import Control.Monad          ( void )
import Control.Monad.Identity ( Identity )
import Data.Char              ( isDigit, toUpper )

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String ( Parser )

import PLTL ( PLTL )
import qualified PLTL

parseSpot :: String -> Either String PLTL
parseSpot str =
  case parseWithWhitespace parseSpot' str of
    Left err   -> Left $ show err
    Right pltl -> Right pltl

parseSpot' :: Parser PLTL
parseSpot' = buildExpressionParser spotTable term

term :: Parser PLTL
term = parentheses <|> try constant <|> literal

spotTable :: OperatorTable String () Identity PLTL
spotTable = [ [ prefix unary ]
            , [ Infix binaryTemporal AssocRight ]
            , [ Infix opAnd AssocLeft ]
            , [ Infix opOr AssocLeft ]
            , [ Infix opXor AssocLeft ]
            , [ Infix (opImpl <|> opIff) AssocRight ]
            ]
  where prefix p = Prefix . chainl1 p $ return (.)

parentheses :: Parser PLTL
parentheses = do
  void $ lexeme $ char '('
  exp <- lexeme $ parseSpot'
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
  if (isReserved p || isDigit (head p)) then
    fail $ "To use '" ++ p ++ "' as a proposition, enclose it in quotation marks."
  else
    return (PLTL.Prop PLTL.NotQuoted p)

-- Reserved strings --

isReserved :: String -> Bool
isReserved str = elem (fmap toUpper str) ["TRUE", "FALSE"] || elem str reservedOps
  where reservedOps =
          [ "xor"
          , "F", "G", "U", "V", "M", "R", "W", "X"
          , "O", "H", "S", "Z", "Y", "T"
          ]

-- Constants --

constant :: Parser PLTL
constant = true <|> false

true :: Parser PLTL
true = do
  lexeme $
    choice
      [ void (char '1')
      , void (mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "true")
      ]
  return PLTL.T

false :: Parser PLTL
false = do
  lexeme $
    choice
      [ void (char '0')
      , void (mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "false")
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
  strings ["!", "~"]
  return (PLTL.UnOp PLTL.Not)

opHist :: Parser (PLTL -> PLTL)
opHist = unaryOrLiteral PLTL.Hist "[~]" 'H'

opOnce :: Parser (PLTL -> PLTL)
opOnce = unaryOrLiteral PLTL.Once "<~>" 'O'

opPrev :: Parser (PLTL -> PLTL)
opPrev = unaryOrLiteral PLTL.Prev "(~)" 'Y'

opWPrev :: Parser (PLTL -> PLTL)
opWPrev = try (lexeme (char 'T' >> notFollowedBy digit >> return (PLTL.UnOp PLTL.WPrev)))

opNext :: Parser (PLTL -> PLTL)
opNext = unaryOrLiteral PLTL.Next "()" 'X'

opGlob :: Parser (PLTL -> PLTL)
opGlob = unaryOrLiteral PLTL.Glob "[]" 'G'

opFut :: Parser (PLTL -> PLTL)
opFut = unaryOrLiteral PLTL.Fut "<>" 'F'

unaryOrLiteral :: PLTL.UnOp -> String -> Char -> Parser (PLTL -> PLTL)
unaryOrLiteral op unaryString maybeUnaryChar = isUnary <|> maybeUnary
  where isUnary = strings [unaryString] >> return (PLTL.UnOp op)
        maybeUnary = try (lexeme (char maybeUnaryChar >> notFollowedBy digit >> return (PLTL.UnOp op)))

-- Binary operators --

binaryTemporal :: Parser (PLTL -> PLTL -> PLTL)
binaryTemporal =
  choice
    [ opUntil
    , opWUntil
    , opRelease
    , opSRelease
    , opSince
    , opWSince
    ]

opAnd :: Parser (PLTL -> PLTL -> PLTL)
opAnd = do
  strings ["&&", "&", "/\\"]
  return (PLTL.BinOp PLTL.And)

opOr :: Parser (PLTL -> PLTL -> PLTL)
opOr = do
  strings ["||", "|", "\\/"]
  return (PLTL.BinOp PLTL.Or)

opImpl :: Parser (PLTL -> PLTL -> PLTL)
opImpl = do
  strings ["-->", "->", "=>"]
  return (PLTL.BinOp PLTL.Impl)

opIff :: Parser (PLTL -> PLTL -> PLTL)
opIff = do
  strings ["<-->", "<->", "<=>"]
  return (PLTL.BinOp PLTL.Iff)

opXor :: Parser (PLTL -> PLTL -> PLTL)
opXor = do
  strings ["^", "xor"]
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
  strings ["R", "V"]
  return (PLTL.BinOp PLTL.Release)

opSRelease :: Parser (PLTL -> PLTL -> PLTL)
opSRelease = do
  strings ["M"]
  return (PLTL.BinOp PLTL.SRelease)

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
