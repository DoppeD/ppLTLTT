{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ParserHOA ( parseHOA ) where

import Control.Monad       ( void )
import Data.Char           ( toUpper )

import Text.Parsec
import Text.Parsec.String  ( Parser )

import qualified HOA

parseHOAS :: Parser [HOA.HOA]
parseHOAS = do
  header <- manyTill header bodyStart
  skipMany bodyStart
  nextLine
  body <- manyTill body bodyEnd
  skipMany anyToken
  return $ filter ((/=) HOA.Other) header ++ body

header :: Parser HOA.HOA
header = try start <|> try ap <|> other

other :: Parser HOA.HOA
other = manyTill anyChar (char '\n') >> return HOA.Other

bodyStart :: Parser ()
bodyStart = mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "--body--"

bodyEnd :: Parser ()
bodyEnd = mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "--end--"

body :: Parser HOA.HOA
body = try state <|> transition

transition :: Parser HOA.HOA
transition = do
  void $ lexeme $ char '['
  val <- sepBy1 valuation (lexeme (char '&'))
  void $ lexeme $ char ']'
  st <- lexeme $ many1 digit
  nextLine
  return $ HOA.Trans val (read st)

valuation :: Parser HOA.Valuation
valuation = trueProp <|> falseProp

trueProp :: Parser HOA.Valuation
trueProp = do
  var <- lexeme $ many1 digit
  return $ HOA.TrueProp $ read var

falseProp :: Parser HOA.Valuation
falseProp = do
  void $ lexeme $ char '!'
  var <- lexeme $ many1 digit
  return $ HOA.FalseProp $ read var

state :: Parser HOA.HOA
state = do
  mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "state"
  void $ lexeme $ char ':'
  st <- lexeme $ many1 digit
  nextLine
  return $ HOA.State $ read st

start :: Parser HOA.HOA
start = do
  mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "start"
  void $ lexeme $ char ':'
  st <- lexeme $ many1 digit
  nextLine
  return $ HOA.StartState $ read st

ap :: Parser HOA.HOA
ap = do
  mapM_ (\a -> lexeme (char a <|> char (toUpper a))) "ap"
  void $ lexeme $ char ':'
  void $ lexeme $ many1 digit
  vars <- lexeme $ many1 apVar
  nextLine
  return $ HOA.AP vars

apVar :: Parser String
apVar = do
  void $ lexeme $ char '"'
  var <- lexeme $ many1 $ satisfy ((/=) '"')
  void $ lexeme $ char '"'
  return var

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

nextLine :: Parser ()
nextLine = void $ many $ oneOf " \n\t"

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseHOA :: String -> Either String [HOA.HOA]
parseHOA hoa =
  case parseWithWhitespace parseHOAS hoa of
    Left err -> Left $ show err
    Right hoa -> Right hoa
