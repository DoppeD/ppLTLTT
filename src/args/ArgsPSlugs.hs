{-# OPTIONS -Wunused-imports -Wincomplete-patterns #-}

module ArgsPSlugs ( PSlugsArgs(..), parseArgs ) where

import Common

import Options.Applicative
import Options.Applicative.Types ( readerAsk )

data PSlugsArgs = PSlugsArgs
  { abstractionPreference :: AbstractionPreference
  , stateEncoding         :: StateEncoding
  , unknownArguments      :: [UnknownArg]
  }

parsePSlugsArgs :: Parser PSlugsArgs
parsePSlugsArgs =
  PSlugsArgs
    <$> flag BreakAtBoolean IncludeBoolean ( long "include-boolean" <> help "Abstract maximal pure-past subformulae, including Boolean connectives." )
    <*> flag Binary OneHot ( long "onehot" <> help "Use one-hot encoding of temporal testers' states" )
    <*> some ( argument unknown ( help "Arguments to forward to SLUGS and files" <> metavar "FILES|ARGS" ))

unknown :: ReadM UnknownArg
unknown = do
  arg <- readerAsk
  if (take 1 arg == "-") then
    return $ Option arg
  else
    return $ Argument arg

parseArgs :: IO PSlugsArgs
parseArgs = execParser opts
  where
    opts = info (parsePSlugsArgs <**> helper)
      ( fullDesc
     <> header "pSLUGS: A wrapper for SLUGS for handling pure-past subformulae"
     <> forwardOptions )
