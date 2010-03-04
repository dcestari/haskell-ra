module GenericOperations where

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
