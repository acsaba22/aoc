module Main (main) where

import Data.List.Extra (allSame, chunksOf, split, trim)
-- import qualified Data.List.Extra as LE
import Util (traceLabel)

parse :: String -> [(Int, Int)]
parse fcontent = pairs
  where
    intervals = traceLabel "i" $ split (== ',') $ trim fcontent

    parseInterval :: String -> (Int, Int)
    parseInterval s = case split (== '-') s of
      [x, y] -> (read x, read y)
      _ -> error "bad pair"
    pairs = map parseInterval intervals

isRepeat :: Int -> Int -> Bool
isRepeat repeatNum x
  | (length s) `mod` repeatNum /= 0 = False
  | otherwise = allSame $ chunksOf ((length s) `div` repeatNum) s
  where
    s = show x

isRepeatAny :: Int -> Bool
isRepeatAny x = any (`isRepeat` x) [2 .. (length (show x))]

solve :: (Int -> Bool) -> [(Int, Int)] -> Int
solve predicate pairs = repeatSum
  where
    repeatNum :: (Int, Int) -> Int
    repeatNum (x, y) =
      let (xx, yy) = traceLabel "Interval" (x, y)
          oks = traceLabel "Repeats" $ filter predicate [xx .. yy]
       in sum oks

    repeatNums = traceLabel "repeatNums" $ map repeatNum pairs

    repeatSum = sum repeatNums

main :: IO ()
main = do
  input <- readFile "d02/input.txt"
  putStrLn $ show input
  let pairs = parse input

  -- lehet parameterbe is tenni
  let !p1 = solve (isRepeat 2) pairs
  let !p2 = solve (isRepeatAny) pairs
  putStrLn $ "!!!!!!! P1 " ++ show p1
  putStrLn $ "!!!!!!! P2 " ++ show p2

-- !!!!!!! P1 30608905813
-- !!!!!!! P2 31898925685
