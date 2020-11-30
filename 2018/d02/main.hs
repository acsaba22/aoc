import Data.List

-- Counts consecutive equal characters
uniqNum :: [Char] -> [Int]
-- uniqNum [] = []
-- uniqNum [c] = [1]
-- uniqNum (x:xs@(y:ys))
--   | x == y = 1 + head rest : tail rest
--   | otherwise = 1:rest
--   where rest = uniqNum xs
uniqNum s = map length $ group s

has2or3 :: [Char] -> (Bool, Bool)
has2or3 s =
  let counts = uniqNum . sort $ s
  in (2 `elem` counts, 3 `elem` counts)


countTrue :: [Bool] -> Int
countTrue = length . filter id
p1 content =
  let (bool2s, bool3s) = unzip . map has2or3 . lines $ content
  in countTrue bool2s * countTrue bool3s

-- skips char k. index starts at 1
-- TODO Misi kerdes: ugy erzem ebben a nyelvben 1-tol indexelunk. pl. [0..n-1] csunya, szoval valoszinu inkabb [1..n]-et hasznalnak.
-- Igy van ez? Eroltessem 0-tol vagy hasznaljak 1-et?
skipK :: Int -> String -> String
skipK k []  = error "Ran out of numbers"
skipK 1 (x:xs) = xs
skipK k (x:xs) = x : skipK (k-1) xs


-- removes all 1st chars, then removes all 2nd cars, and so on.
genRemoved :: [String] -> [[String]]
genRemoved ss =
  let n = length $ head ss
  in [map (skipK k) ss | k <- [1..n]]

findDups :: [String] -> [String]
findDups ss =
  let grouped = group . sort $ ss
  in map head $ filter (\g -> 2 <= length g) grouped

-- TODO Misi kerdes: szeretnek reszeredmenyeket latni egymas mellett, muszaj IO?
-- most azt csinalom hogy valtoztatgatom a programot es a consolba latom az elozo futas vegeredmenyet.
-- Van valami mas workaround?
p2 content =
  head $ foldl (++) [] $ (map findDups) . genRemoved . lines $ content

main = do
  content <- readFile "input.txt"
  putStr "P1 = "
  print $ p1 content
  putStr "P2 = "
  print $ p2 content
