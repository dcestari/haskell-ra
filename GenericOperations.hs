module GenericOperations where

trim :: [Int] -> [Int]
trim [] = []
trim (x:xs) = if x == 0 then trim xs else (x:xs)

rtrim :: [Int] -> [Int]
rtrim xs = if trimmed == [0] then [] else trimmed
  where trimmed = reverse (trim (reverse xs))

normalize :: [Int] -> [Int] -> ([Int], [Int], Int)
normalize xs ys = (xs ++ (zpad n), ys ++ (zpad m), max n m)
  where n = length xs
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

greater :: [Int] -> [Int] -> Bool
greater [] [] = True
greater (x:xs) (y:ys)
  | x == y = greater xs ys
  | x > y  = True
  | x < y  = False
