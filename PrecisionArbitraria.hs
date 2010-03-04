module PrecisionArbitraria where

import RA
import RA.Base
import RA.Utils

import GenericOperations

main :: IO ()
main =
  let a = Neg [3,5] [] 10
      b = Neg [5] [] 10
  in putStrLn ((showRA a) ++ " / " ++ (showRA b) ++ " = " ++ (showRA (divRA a b 1)))
