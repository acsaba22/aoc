factorial :: Integer -> Integer
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)

pow2 :: Integer -> Integer
pow2 n = n * n
