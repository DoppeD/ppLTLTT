{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ArgsPltl2tgba ( PSpotArgs(..), parseArgs ) where

import Common

import Options.Applicative
import Options.Applicative.Types ( readerAsk )

data PSpotArgs = PSpotArgs
  { abstractionPreference  :: AbstractionPreference
  , unknownArguments       :: [UnknownArg]
  , inputType              :: InputType
  }

parsePSpotArgs :: Parser PSpotArgs
parsePSpotArgs =
  PSpotArgs
    <$> flag BreakAtBoolean IncludeBoolean ( long "include-boolean" <> help "Abstract subformulae with top-level boolean connectives (if they contain past operators)" )
    <*> many ( argument unknown ( help "Arguments to forward to autfilt" <> metavar "AUTFILT_ARGS" ))
    <*> (parseFormula <|> parseFile)

parseFormula :: Parser InputType
parseFormula = Direct <$> strOption ( long "formula" <> short 'f' <> metavar "FORMULA" <> help "Formula to process" )

parseFile :: Parser InputType
parseFile = FromFile <$> strOption ( long "file" <> short 'F' <> metavar "FILE" <> help "File to process" )

unknown :: ReadM UnknownArg
unknown = do
  arg <- readerAsk
  if (take 1 arg == "-") then
    return $ Option arg
  else
    return $ Argument arg

parseArgs :: IO PSpotArgs
parseArgs = execParser opts
  where
    opts = info (parsePSpotArgs <**> helper)
      ( fullDesc
     <> header "pltl2tgba: A wrapper for ltl2tgba for handling pure-past subformulae"
     <> forwardOptions )
