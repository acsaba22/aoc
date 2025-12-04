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

stops :: Int -> [String] -> [Int]
stops p [] = [p]
stops p (line : xs) = p' : stops p' xs
  where
    p' = newPos line
    newPos :: String -> Int
    newPos ('L' : nums) = normalize $ p - read nums
    newPos ('R' : nums) = normalize $ p + read nums
    newPos _ = error "L or R"
    normalize = (`mod` 100)

main :: IO ()
main = do
  -- allInput :: String <- readFile "d01/example.txt"
  -- let input = lines allInput

  input <- lines <$> readFile "d01/input.txt"
  print input
  -- mapM_ print input
  mapM_ print $ stops 50 input
  -- print $ acsOsszeado 3 4 5
  let p1 = length $ filter (== 0) $ stops 50 input
  putStrLn $ "!!!!!!! P1 " ++ show p1
