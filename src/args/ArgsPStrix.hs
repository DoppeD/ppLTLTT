{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ArgsPStrix ( PStrixArgs(..), parseArgs ) where

import Common

import Options.Applicative
import Options.Applicative.Types ( readerAsk )

data PStrixArgs = PStrixArgs
  { abstractionPreference :: AbstractionPreference
  , inputVars             :: Maybe String
  , outputVars            :: Maybe String
  , stateEncoding         :: StateEncoding
  , unknownArguments      :: [UnknownArg]
  , inputType             :: InputType
  }

parsePStrixArgs :: Parser PStrixArgs
parsePStrixArgs =
  PStrixArgs
    <$> flag BreakAtBoolean IncludeBoolean ( long "include-boolean" <> help "Abstract maximal pure-past subformulae, including Boolean connectives." )
    <*> optional ( strOption ( long "ins" <> internal <> hidden ))
    <*> optional ( strOption ( long "outs" <> internal <> hidden ))
    <*> flag Binary OneHot ( long "onehot" <> help "Use one-hot encoding of temporal testers' states" )
    <*> many ( argument unknown ( help "Arguments to forward to Strix" <> metavar "STRIX_ARGS" ))
    <*> (parseFormula <|> parseFile)

parseFormula :: Parser InputType
parseFormula = Direct <$> strOption ( long "formula" <> short 'f' <> metavar "FORMULA" <> help "Formula to process" )

parseFile :: Parser InputType
parseFile = FromFile <$> strOption ( long "formula-file" <> short 'F' <> metavar "FILE" <> help "File to process" )

unknown :: ReadM UnknownArg
unknown = do
  arg <- readerAsk
  if (take 1 arg == "-") then
    return $ Option arg
  else
    return $ Argument arg

parseArgs :: IO PStrixArgs
parseArgs = execParser opts
  where
    opts = info (parsePStrixArgs <**> helper)
      ( fullDesc
     <> header "pStrix: A wrapper for Strix for handling pure-past subformulae"
     <> forwardOptions )
