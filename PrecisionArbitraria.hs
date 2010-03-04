-------------------------------------------------
-- Universidad Simón Bolívar
-- Laboratorio de Lenguajes de Programación
-- Proyecto 2: Aritmética de Precisión Arbitraria
--
-- Elaborado por:
--  Daniel Cestari - 04-36834
--
--   Implementación de una librería para el cálculo
-- con precisión arbitraria
--
--   Se define el tipo RealArbitrario como dos listas
-- de enteros (parte entera y parte decimal) y un
-- entero adicional para la base.
-- 
--  Cada elemento de la lista debe ser un entero menor
-- a la base (dígito). La parte entera se ordena de
-- menos significativo a más significativo y la parte
-- decimal en orden inverso.
--
-- Ejemplo:
--  (-> se lee "se representa como")
--   250,54 -> NoNeg [0,5,2] [5,4] 10
--   -25    -> NoNeg [5,2] [] 10
--    0     -> NoNeg [] [] 10
--
-------------------------------------------------

module PrecisionArbitraria where

-- BEGIN: module GenericOperations where

  trim :: [Int] -> [Int]
  trim [] = []
  trim list = if x == 0 then trim (reverse xs) else list
    where (x:xs) = reverse list

  normalize :: [Int] -> [Int] -> ([Int], [Int], Int)
  normalize a b = (xs ++ (zpad n), ys ++ (zpad m), max n m)
    where xs = trim a
          ys = trim b
          n = length xs
          m = length ys
          z = repeat 0
          zpad = \k -> (take ((max n m) - k) z)

  addWithCarry :: [Int] -> [Int] -> Int -> Int -> [Int]
  addWithCarry [] [] b k
    | k > 0     = [k]
    | otherwise = []
  addWithCarry (x:xs) (y:ys) b k =
    let d = x + y + k
    in [d `mod` b] ++ (addWithCarry xs ys b (d `div` b))

  subWithBorrow :: [Int] -> [Int] -> Int -> Int -> [Int]
  subWithBorrow [] [] b k
    | k > 0     = error "try substracting in the opposite order"
    | otherwise = []
  subWithBorrow (x:xs) (y:ys) b k
    | d < 0 = r 1
    | otherwise = r 0
    where d = x - y - k
          r = \borrow -> ([d `mod` b] ++ (subWithBorrow xs ys b borrow))

  multWithCarry :: [Int] -> [Int] -> Int -> [Int]
  multWithCarry _ [] b = []
  multWithCarry xs (y:ys) b =
    let (w, z, d) = normalize (vectorMultWithCarry y xs b 0) (0 : multWithCarry xs ys b)
    in addWithCarry w z b 0

  bruteForceDiv :: [Int] -> [Int] -> Int -> Int -> Int -> ([Int], Int)
  bruteForceDiv xs [] b d ds = error "infinite"
  bruteForceDiv [] ys b d ds = ([0], ds)
  bruteForceDiv xs ys b d ds
    | isZero xs = ([0], ds)
    | greater xs ys =
      let left = subWithBorrow xs ys b 0
          (times, decimales) = bruteForceDiv left ys b d ds
          (w, z, _) = normalize times ((take (decimales - ds) (repeat 0)) ++ [1])
      in (addWithCarry w z b 0, decimales)
    | greater ys xs =
      if d > 0 then
        let (w, z, _) = normalize (0:xs) ys
            (times, decimales) = bruteForceDiv w z b (d - 1) (ds + 1)
            s = if ds > 0 then [0] else []
        in ((times ++ s), decimales)
      else
        ([0], ds)
    | otherwise = ([1], ds)

  isZero :: [Int] -> Bool
  isZero [] = True
  isZero (x:xs) = and [x == 0, isZero xs]

  vectorMultWithCarry :: Int -> [Int] -> Int -> Int -> [Int]
  vectorMultWithCarry _ [] _ k
    | k > 0 = [k]
    | otherwise = []
  vectorMultWithCarry c (x:xs) b k =
    let d = c * x + k
    in [d `mod` b] ++ (vectorMultWithCarry c xs b (d `div` b))

  greater :: [Int] -> [Int] -> Bool
  greater [] [] = True
  greater a b
    | x == y = greater (reverse xs) (reverse ys)
    | x > y  = True
    | x < y  = False
    where (x:xs) = reverse a
          (y:ys) = reverse b

  carry :: [Int] -> Int -> [Int]
  carry [] b = []
  carry (x:xs) b = [x `mod` b] ++ (carry xs b)

-- END: module GenericOperations where

-- BEGIN: module RA.Base where

  data RealArbitrario
    = NoNeg [Int] [Int] Int
    | Neg [Int] [Int] Int
    deriving (Eq, Show)

-- END: module RA.Base where

-- BEGIN: module RA.Utils where

--  import RA.Base
--  import GenericOperations

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

  showParteEntera :: RealArbitrario -> String
  showParteEntera (NoNeg (x:xs) ys b) = (showParteEntera (NoNeg xs ys b)) ++ (show x)
  showParteEntera (NoNeg [] ys b) = []
  showParteEntera (Neg (x:xs) ys b) = "-" ++ (showParteEntera (NoNeg xs ys b)) ++ (show x)
  showParteEntera (Neg [] ys b) = []

  showDecimales :: RealArbitrario -> String
  showDecimales (NoNeg xs ys b) = showDecimales (Neg xs ys b)
  showDecimales (Neg xs (y:ys) b)
    | y == 0 = if decimales == "" then "" else (show y) ++ decimales
    | otherwise = (show y) ++ decimales
    where decimales = showDecimales (Neg xs ys b)
  showDecimales (Neg xs [] b) = []

  isNegative :: RealArbitrario -> Bool
  isNegative (NoNeg _ _ _) = False
  isNegative (Neg _ _ _) = True

  trimRA :: RealArbitrario -> RealArbitrario
  trimRA (NoNeg xs ys b) = NoNeg (trim xs) (trim ys) b
  trimRA (Neg xs ys b) = Neg (trim xs) (trim ys) b

-- END: module RA.Utils where

-- BEGIN: module RA where

--  import GenericOperations
--  import RA.Base
--  import RA.Utils

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

  base10RA :: RealArbitrario -> RealArbitrario
  base10RA num@(Neg _ _ 10) = num
  base10RA num@(NoNeg _ _ 10) = num
  base10RA num@(Neg xs ys b) =
    let (NoNeg ws zs _) = base10RA (NoNeg xs ys b)
    in Neg ws zs 10
  base10RA (NoNeg xs ys b) =
    let entera = changeBaseTo10RA (map (\y -> NoNeg [y] [] 10) xs) (NoNeg (carry [b] 10) [] 10)
        decimal = changeBaseTo10RA ([NoNeg [] [] 10] ++ (map (\y -> NoNeg [y] [] 10) ys)) (divRA (NoNeg [1] [] 10) (NoNeg (carry [b] 10) [] 10) 10)
    in sumaRA entera decimal

  changeBaseTo10RA :: [RealArbitrario] -> RealArbitrario -> RealArbitrario
  changeBaseTo10RA [] _ = NoNeg [] [] 10
  changeBaseTo10RA (x:xs) b =
    let digits = map (\y -> multRA b y) xs
        rest = changeBaseTo10RA digits b
    in sumaRA x rest

  showRA :: RealArbitrario -> String
  showRA num@(Neg xs ys 10) = "-" ++ (showRA (NoNeg xs ys 10))
  showRA num@(NoNeg _ _ 10) =
    let r = trimRA num
        entera = showParteEntera r
        decimal = showDecimales r
        e_str = if entera == "" then (if isNegative r then "-" else "") ++ "0" else entera
        d_str = if decimal == "" then "" else "." ++ decimal
    in e_str ++ d_str
  showRA num = showRA (base10RA num)

-- END: module RA where
