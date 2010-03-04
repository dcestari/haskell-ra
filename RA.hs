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
