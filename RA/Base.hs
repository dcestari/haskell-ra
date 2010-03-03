module RA.Base where

data RealArbitrario
  = NoNeg [Int] [Int] Int
  | Neg [Int] [Int] Int
  deriving (Eq, Show)
