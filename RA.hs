module RA where

import GenericOperations
import RA.Base
import RA.Utils

sumaRA :: RealArbitrario -> RealArbitrario -> RealArbitrario
sumaRA x@(Neg _ _ _) y@(NoNeg _ _ _) = restaRA y x
sumaRA x@(NoNeg _ _ _) y@(Neg _ _ _) = restaRA x y
sumaRA x@(Neg xs ys b1) y@(Neg ws zs b2)
  | b1 == b2 =
    let (a, b, d) = toList x y
        s = (addWithCarry a b b1 0)
    in (fromList s d b1 True)
  | otherwise = error "sólo se pueden sumar números con la misma base"
sumaRA x@(NoNeg xs ys b1) y@(NoNeg ws zs b2)
  | b1 == b2 =
    let (a, b, d) = toList x y
        s = (addWithCarry a b b1 0)
    in (fromList s d b1 False)
  | otherwise = error "sólo se pueden sumar números con la misma base"

restaRA :: RealArbitrario -> RealArbitrario -> RealArbitrario
restaRA x@(Neg xs ys b1) y@(Neg ws zs b2) = restaRA (NoNeg ws zs b2) x
restaRA x@(NoNeg xs ys b1) y@(Neg ws zs b2) = sumaRA x (NoNeg ws zs b2)
restaRA x@(Neg xs ys b1) y@(NoNeg ws zs b2) = sumaRA x (Neg ws zs b2)
restaRA x@(NoNeg xs ys b1) y@(NoNeg ws zs b2)
  | b1 == b2 =
    let (a, b, d) = toList x y
        neg = not (greater a b)
        s = if not neg then (subWithBorrow a b b1 0) else (subWithBorrow b a b1 0)
    in (fromList s d b1 neg)
  | otherwise = error "sólo se pueden restar números con la misma base"

multRA :: RealArbitrario -> RealArbitrario -> RealArbitrario
multRA x@(Neg xs ys b1) y@(Neg ws zs b2) = multRA (NoNeg xs ys b1) (NoNeg ws zs b2)
multRA x@(Neg _ _ _) y@(NoNeg _ _ _) = multRA y x
multRA x@(NoNeg _ _ _) y@(Neg ws zs b) =
  let (NoNeg xs ys _) = multRA x (NoNeg ws zs b)
  in Neg xs ys b
multRA x@(NoNeg xs ys b1) y@(NoNeg ws zs b2)
  | b1 == b2 =
    let (a, b, _) = toList x y
        d = max (length ys) (length zs)
        s = multWithCarry a b b1
    in fromList s (d * 2) b1 False
  | otherwise = error "sólo se pueden multiplicar números con la misma base"

divRA :: RealArbitrario -> RealArbitrario -> Int -> RealArbitrario
divRA x@(Neg xs ys b1) y@(Neg ws zs b2) d = divRA (NoNeg xs ys b1) (NoNeg ws zs b2) d
divRA x@(Neg xs ys b) y@(NoNeg _ _ _) d =
  let (NoNeg ws zs _) = divRA (NoNeg xs ys b) y d
  in Neg ws zs b
divRA x@(NoNeg _ _ _) y@(Neg ws zs b) d =
  let (NoNeg xs ys _) = divRA x (NoNeg ws zs b) d
  in Neg xs ys b
divRA x@(NoNeg xs ys b1) y@(NoNeg ws zs b2) d
  | b1 == b2 =
    let (a, b, _) = toList x y
        (s, decimales) = bruteForceDiv a b b1 d 0
    in fromList s decimales b1 False
  | otherwise = error "sólo se pueden dividir números con la misma base"

piRA :: Int -> Int -> RealArbitrario
piRA n d
  | n < 0 = NoNeg [] [] 10
  | otherwise = 
    sumaRA (piRA (n - 1) d) (multRA (divRA (NoNeg [1] [] 10) (powRA (NoNeg [6,1] [] 10) n) d)
    (restaRA (restaRA (restaRA (term 4 1) (term 2 4)) (term 1 5)) (term 1 6)))
  where term = \a b -> (divRA (NoNeg [a] [] 10) (sumaRA (powRA (NoNeg [8] [] 10) n) (NoNeg [b] [] 10)) d)

powRA :: RealArbitrario -> Int -> RealArbitrario
powRA num times = (pow num) !! times

pow num@(NoNeg _ _ b) = NoNeg [1] [] b : num : [ (multRA a num) | a <- (tail (pow num))]
pow num@(Neg _ _ b) = NoNeg [1] [] b : num : [ (multRA a num) | a <- (tail (pow num))]
