module Main (main) where

import Data.List (findIndex, sort)
import Util (traceLabel)

type Interval = (Int, Int)

parseInterval :: String -> Interval
parseInterval s = ((read as), (read bs))
  where
    (as, bs') = break (== '-') s
    bs = tail bs' -- remove '-'

inInterval :: Int -> Interval -> Bool
inInterval x (a, b) = (a <= x) && (x <= b)

solveP1 :: [Int] -> [Interval] -> Int
solveP1 ids intervals = ret
  where
    fresh :: Int -> Bool
    fresh x = any (inInterval x) intervals --- TODO id foglalt, tul sok foglalt
    ret = length $ filter fresh ids

boundOpen :: Char
boundOpen = 'a'
boundClose :: Char
boundClose = 'b'

solveP2 :: [Interval] -> Int
solveP2 intervals = ret
  where
    bounds = [(fst i, boundOpen)| i <- intervals] ++ [(1 + (snd i), boundClose)| i <- intervals] :: [(Int, Char)]
    sorted = traceLabel "sorted" $ sort bounds
    intervalSums :: Int -> Int -> Int -> [(Int, Char)] -> Int
    intervalSums partialResult _ _ [] = partialResult
    intervalSums partialResult openCount lastPos ((pos, boundType) : remainingBounds) = ret'
      where
        increase = case openCount of
          0 -> 0
          _ -> pos - lastPos 
        newOpenCount = openCount + (if boundType == boundOpen then 1 else -1)
        ret' = intervalSums (partialResult+increase) newOpenCount pos remainingBounds
    ret = intervalSums 0 0 (-1) sorted :: Int

main :: IO ()
main = do
  input <- lines <$> readFile "d05/input.txt"
  print input
  let maybeIdx = findIndex (== "") input
  case maybeIdx of
    Nothing -> error "No empty line"
    Just idx -> do
      let (intervalStr, restStr) = splitAt idx input
      let ids = map read $ tail restStr :: [Int]
      let intervals = map parseInterval intervalStr
      print $ ids
      print intervals
      let (!p1) = solveP1 ids intervals :: Int
      let (!p2) = solveP2 intervals :: Int
      putStrLn $ "!!!!!!!!! P1: " ++ show p1
      putStrLn $ "!!!!!!!!! P2: " ++ show p2

-- !!!!!!!!! P1: 640
-- !!!!!!!!! P2: 365804144481581
