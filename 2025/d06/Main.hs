module Main (main) where
import Data.List (transpose)
import Data.List.Extra (trim)
import Data.List.Split (splitWhen)


applyOperation :: Char -> [Int] -> Int
applyOperation '+' vals = sum vals
applyOperation '*' vals = product vals
applyOperation _ _ = error "Bad op"


applyAll :: [Char] -> [[Int]] -> [Int]
applyAll (operation : operations) (vals : operands) = result : applyAll operations operands
  where
    result = applyOperation operation vals
applyAll _ _ = []

solve  :: [Char] -> [[Int]] -> Int
solve operations operands = sum $ applyAll operations operands

main :: IO ()
main = do
  input <- lines <$> readFile "d06/input.txt"
  print input
  let tokenized = map words input
  print tokenized
  let operations = map head $ last tokenized :: [Char]
  let operands = transpose $ map (map read) $ init tokenized :: [[Int]]
  print operations
  print operands
  print $ applyAll operations operands

  let p2Input = init input
  print p2Input
  let cols = map trim $ transpose p2Input
  print cols
  let p2Operands = map (map read) $ splitWhen (== "") cols :: [[Int]]
  print p2Operands

  let (!p1) = solve operations operands :: Int
  let (!p2) = solve operations p2Operands :: Int
  putStrLn $ "!!!!!!!!! P1: " ++ show p1
  putStrLn $ "!!!!!!!!! P2: " ++ show p2

-- !!!!!!!!! P1: 6171290547579
-- !!!!!!!!! P2: 8811937976367
