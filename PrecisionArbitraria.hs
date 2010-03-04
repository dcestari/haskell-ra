module PrecisionArbitraria where

import RA
import RA.Base
import RA.Utils

main :: IO ()
main =
  let a = Neg [] [5] 10
      b = Neg [] [5] 10
  in putStrLn ((showRA a) ++ " * " ++ (showRA b) ++ " = " ++ (showRA (multRA a b)))
