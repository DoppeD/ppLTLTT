{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ArgsPPLTLTT ( PPLTLTTArgs(..), parseArgs ) where

import Common

import Options.Applicative

data PPLTLTTArgs = PPLTLTTArgs
  { outputFile      :: Maybe String
  , monitorVariable :: String
  , inputType       :: InputType
  }

parsePPLTLTTArgs :: Parser PPLTLTTArgs
parsePPLTLTTArgs =
  PPLTLTTArgs
    <$> optional ( strOption ( long "output" <> short 'o' <> metavar "FILENAME" <> help "File to output result to" ))
    <*> strOption ( long "monitor-variable" <> short 'm' <> metavar "IDENTIFIER" <> help "Monitor variable" <> value "z" <> showDefault )
    <*> (parseFormula <|> parseFile)

parseFormula :: Parser InputType
parseFormula = Direct <$> strOption ( long "formula" <> short 'f' <> metavar "FORMULA" <> help "Formula to process" )

parseFile :: Parser InputType
parseFile = FromFile <$> strOption ( long "file" <> short 'F' <> metavar "FILE" <> help "File to process" )

parseArgs :: IO PPLTLTTArgs
parseArgs = execParser opts
  where
    opts = info (parsePPLTLTTArgs <**> helper)
      ( fullDesc
     <> header "ppLTLTT: Converts ppLTL-formulae into temporal testers"
     <> forwardOptions )
