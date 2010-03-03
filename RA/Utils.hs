module RA.Utils where

import RA.Base
import GenericOperations

fromList :: [Int] -> Int -> Int -> Bool -> RealArbitrario
fromList list n b neg
  | not neg = NoNeg (drop n list) (reverse (take n list)) b
  | neg = Neg (drop n list) (reverse (take n list)) b

toList :: RealArbitrario -> RealArbitrario -> ([Int], [Int], Int)
toList x@(NoNeg xs ys b1) y@(Neg ws zs b2) = toList x (NoNeg ws zs b2)
toList x@(Neg xs ys b1) y@(NoNeg ws zs b2) = toList (NoNeg xs ys b2) y
toList x@(Neg xs ys b1) y@(Neg ws zs b2) = toList (NoNeg xs ys b2) (NoNeg ws zs b2)
toList x@(NoNeg xs ys b1) y@(NoNeg ws zs b2) =
    let (e1, e2, e) = normalize xs ws
        (d1, d2, d) = normalize ys zs
        a = (reverse d1) ++ e1
        b = (reverse d2) ++ e2
    in (a, b, d)

showRA :: RealArbitrario -> String
showRA (num) =
  let entera = showParteEntera num
      decimal = showDecimales num
  in if decimal == "" then entera else entera ++ "." ++ decimal

showParteEntera :: RealArbitrario -> String
showParteEntera (NoNeg (x:xs) ys b) = (showParteEntera (NoNeg (trim xs) ys b)) ++ (show x)
showParteEntera (NoNeg [] ys b) = []
showParteEntera (Neg (x:xs) ys b) = "-" ++ (showParteEntera (NoNeg (trim xs) ys b)) ++ (show x)
showParteEntera (Neg [] ys b) = []

showDecimales :: RealArbitrario -> String
showDecimales (NoNeg xs ys b) = showDecimales (Neg xs (rtrim ys) b)
showDecimales (Neg xs (y:ys) b)
  | y == 0 = if decimales == "" then "" else (show y) ++ decimales
  | otherwise = (show y) ++ decimales
  where decimales = showDecimales (Neg xs (rtrim ys) b)
showDecimales (Neg xs [] b) = []
