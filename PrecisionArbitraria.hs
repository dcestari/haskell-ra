module PrecisionArbitraria where

import RA
import RA.Base
import RA.Utils

main :: IO ()
main =
  let a = Neg [0,1] [0] 10
      b = Neg [1,1] [0] 10
  in putStrLn ((showRA a) ++ " + " ++ (showRA b) ++ " = " ++ (showRA (sumaRA a b)))
