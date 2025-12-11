module Main (main) where

joltage :: Int -> String -> String
joltage 0 _ = ""
joltage digits s = ret
  where
    -- this is O(n) but there is also a Text or ByteString if needed
    k = length s - digits + 1
    -- better way to find first...
    (maxv, maxp) = maximum $ zip (take k s) (map (* (-1)) [0 :: Int ..])
    ret = [maxv] ++ joltage (digits - 1) (drop (-maxp + 1) s)

main :: IO ()
main = do
  input <- lines <$> readFile "d03/input.txt"
  let solve k = (sum $ map read $ map (joltage k) input) :: Int
  let p1 = solve 2
  let p2 = solve 12
  putStrLn $ "!!!!!!!!! P1: " ++ show p1
  putStrLn $ "!!!!!!!!! P2: " ++ show p2

-- !!!!!!!!! P1: 17403
-- !!!!!!!!! P2: 173416889848394
