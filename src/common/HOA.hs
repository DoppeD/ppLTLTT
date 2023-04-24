module HOA where

data HOA = AP [String]
         | Other
         | StartState Int
         | State Int
         | Trans [Valuation] Int
  deriving (Eq, Show)

data Valuation = TrueProp Int
               | FalseProp Int
  deriving (Eq, Show)
