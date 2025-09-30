module Gentle where
import GHC.Profiling (requestHeapCensus)

-- Try out examples from gentle introduction to haskell

inc :: Integer -> Integer
inc n = n+1

len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + len xs

-- ghci> ptX (Pt 1 2)
-- 1
data Point a = Pt a a

ptX :: Point a -> a
ptX (Pt x _) = x

-- ghci> fringe (Branch (Leaf 1) (Leaf 2))
-- [1,2]
data Tree a = Leaf a | Branch (Tree a) (Tree a)

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch x y) = fringe x ++ fringe y

-- ghci> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- ghci> [1,3..10]
-- [1,3,5,7,9]
-- ghci> [(x,y) | x <- [1..3], y <- [11..13]]
-- [(1,11),(1,12),(1,13),(2,11),(2,12),(2,13),(3,11),(3,12),(3,13)]

-- ghci> quicksort [4,3,6,2,3,7,4,1,9,10,4,4,5,6]
-- [1,2,3,3,4,4,4,4,5,6,6,7,9,10]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <-xs, y < x]
                 ++ [x]
                 ++ quicksort [y | y <- xs, x <= y]

add :: Integer -> Integer -> Integer
add x y = x + y

incV2 = add 1

-- ghci> map (add 10) [1,2,3,4]
-- [11,12,13,14]

concatV2 :: [a] -> [a] -> [a]
concatV2 [] y = y
concatV2 (x:xs) y = x : concatV2 xs y
-- ghci> concatV2 "Hello " "world!"
-- "Hello world!"
-- ghci> "Hello " `concatV2` "world!"
-- "Hello world!"
-- ghci> "Hello " ++ "world!"
-- "Hello world!"
-- ghci> (++) "Hello " "world!"
-- "Hello world!"

incV3 = (1 + )
incV4 = ( + 1)

bot = bot

-- ghci> map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]
-- ghci> take 10 (map (^2) [1..])
-- [1,4,9,16,25,36,49,64,81,100]
-- ghci> zip [1,2,3,4] [11,12,13]
-- [(1,11),(2,12),(3,13)]

fibV0 :: Integer -> Integer
fibV0 0 = 1
fibV0 1 = 1
fibV0 n = fibV0 (n-2) + fibV0 (n-1)

fibV0Seq :: [Integer]
fibV0Seq = [fibV0 x | x <- [1..]]

fib31 = take 31 fibV0Seq

ratios :: [Integer] -> [Double]
ratios s@(x:xs) = map (\(x,y) -> fromInteger x / fromInteger y) (zip xs s)

ratio30 = take 30 (ratios fibV0Seq)

fibV1 :: [Integer]
fibV1 = 1 : 1 : [(a+b) | (a,b) <- zip fibV1 (tail fibV1)]

-- sign :: Integer -> Integer
sign x
  | 0 < x = 1
  | x == 0 = 0
  | x < 0 = -1

reqs = client firstReq resps
resps = server reqs

client firstReq ~(resp:resps) = firstReq: client (next resp) resps
server (req:reqs) = process req : server reqs

firstReq = 0
next resp = resp
process req = req+1

fibV2@(1:tfib) = 1 : 1 : [ a+b | (a,b) <- zip fibV2 tfib]

fibV3@(1:tfib3) = 1 : 1 : [ a+b | (a,b) <- zip fibV3 tfib3]