module Main (main) where

-- import Debug.Trace (traceShowId)
-- acsOsszeado :: Int -> Int -> Int -> Int
-- acsOsszeado a b c = a - traceShowId (b + c)

-- TRY 1
-- newPos :: String -> Int -> Int
-- newPos ('L' : _) p = p - 1
-- newPos ('R' : _) p = p + 1
-- newPos _ _ = error "L or R"

-- -- TODO named parameters --- NO max wrapper function
-- stops :: [String] -> Int -> [Int]
-- stops = flip $ scanl (flip newPos)
-- -- stops [] p = [p]
-- -- stops (x : xs) p = p' : stops xs p'
-- --   where
-- --     p' = newPos x p

-- -- stops lines pos = case lines of
-- --   [] -> [pos]
-- --   x:xs ->

-- TODO inner function
-- TODO [1] for tuple?
-- normalize :: Int -> Int
-- normalize x = x `mod` 100

-- newPos :: Int -> String -> Int
-- newPos p ('L' : nums) = normalize $ p - read nums
-- newPos p ('R' : nums) = normalize $ p + read nums
-- newPos _ _ = error "L or R"

stops :: Int -> [String] -> [(Int, Int)]
stops p [] = [(p, 0)]
stops pos (line : xs) = (pos'final, p2part) : stops pos'final xs
  where
    pos'raw = newPos line
    pos'final = pos'raw `mod` 100
    rounds = pos'raw `div` 100
    p2part = (
      (abs rounds)
      + (if pos == 0 && rounds < 0 then -1 else 0)
      + (if pos'final == 0 && 0 < rounds then -1 else 0))
    newPos :: String -> Int
    newPos ('L' : nums) = pos - read nums
    newPos ('R' : nums) = pos + read nums
    newPos _ = error "L or R"

main :: IO ()
main = do
  -- allInput :: String <- readFile "d01/example.txt"
  -- let input = lines allInput

  input <- lines <$> readFile "d01/input.txt"
  print input
  -- mapM_ print input
  mapM_ print $ stops 50 input
  -- print $ acsOsszeado 3 4 5
  let s = stops 50 input
  let p1 = length $ filter (== 0) $ map fst s
  putStrLn $ "!!!!!!! P1 " ++ show p1
  let p2 = p1 + (sum $ map snd s)
  putStrLn $ "!!!!!!! P2 " ++ show p2
-- !!!!!!! P1 1066
-- !!!!!!! P2 6223
