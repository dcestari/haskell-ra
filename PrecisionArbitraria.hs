module PrecisionArbitraria where

data RealArbitrario
  = NoNeg [Int] [Int] Int
  | Neg [Int] [Int] Int
  deriving (Eq, Show)

main :: IO ()

main =
  let a = Neg [0,1] [0] 10
      b = Neg [1,1] [0] 10
  in putStrLn ((showRA a) ++ " + " ++ (showRA b) ++ " = " ++ (showRA (sumaRA a b)))

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

normalize :: [Int] -> [Int] -> ([Int], [Int], Int)
normalize xs ys = (xs ++ (zpad n), ys ++ (zpad m), max n m)
  where n = length xs
        m = length ys
        z = repeat 0
        zpad = \k -> (take ((max n m) - k) z)

restaRA :: RealArbitrario -> RealArbitrario -> RealArbitrario
restaRA x@(NoNeg xs ys b1) y@(NoNeg ws zs b2)
  | b1 == b2 =
    let (a, b, d) = toList x y
        neg = not (greater a b)
        s = if not neg then (subWithBorrow a b b1 0) else (subWithBorrow b a b1 0)
    in (fromList s d b1 neg)
  | otherwise = error "sólo se pueden restar números con la misma base"

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

greater :: [Int] -> [Int] -> Bool
greater [] [] = True
greater (x:xs) (y:ys)
  | x == y = greater xs ys
  | x > y  = True
  | x < y  = False

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

trim :: [Int] -> [Int]
trim [] = []
trim (x:xs) = if x == 0 then trim xs else (x:xs)

rtrim :: [Int] -> [Int]
rtrim xs = if trimmed == [0] then [] else trimmed
  where trimmed = reverse (trim (reverse xs))

