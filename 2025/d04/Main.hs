module Main (main) where

import Data.Array (Array, Ix (inRange), bounds, elems, indices, listArray, (!))

type Coor = (Int, Int)

data Map = Map
  { -- TODO replace with size Ix? Inclusive? maybe use bounds
    matrix :: Array Coor Char,
    height :: Int,
    width :: Int
  }

-- TODO hasznaljak {-# LANGUAGE RecordDotSyntax #-} -ot?
-- can I do show m = "Map{" ++ (show m.height) ++ ", " ++ (show m.width) ++ "}"
-- show m = "Map{" ++ show (height m) ++ ", " ++ show (width m) ++ "}"
instance Show Map where
  show Map {matrix = mat, height, width} = unlines [ 
    [mat ! (y, x) | x <- [0 .. height - 1]] | y <- [0 .. width - 1]]

mapEmpty :: Char
mapEmpty = '.'

valAt :: Map -> Coor -> Char
valAt m c
  | (inRange $ bounds $ matrix m) c = matrix m ! c
  | otherwise = mapEmpty -- constant example, should be '.'

neigh8 :: Coor -> [Coor]
neigh8 (y, x) =
  [ (y - 1, x - 1),
    (y - 1, x),
    (y - 1, x + 1),
    (y, x - 1),
    (y, x + 1),
    (y + 1, x - 1),
    (y + 1, x),
    (y + 1, x + 1)
  ]

removeOuter :: Map -> (Int, Map)
removeOuter m@(Map mat w h) = (removeCount, newMap)
  where
    isRoll :: Coor -> Int
    isRoll coor = if valAt m coor == '@' then 1 else 0
    neighCount :: Coor -> Int
    neighCount coor = sum $ map isRoll $ neigh8 coor
    newVal :: Coor -> Char
    newVal c = case valAt m c of
      '@' -> if (neighCount c) < 4 then 'x' else '@'
      _ -> '.'
    b = bounds mat
    newMatrix = listArray b $ map newVal (indices mat)
    newMap = Map (newMatrix) h w
    removeCount = sum [1 | c <- elems newMatrix, c == 'x']

parseLines :: [String] -> Map
parseLines ls = Map m h w
  where
    h = length ls
    w = length $ head ls
    m = listArray ((0, 0), (h - 1, w - 1)) $ concat ls

removeAll :: Map -> [(Int, Map)]
removeAll m = case removeCount of
  0 -> []
  _ -> (removeCount, newM) : removeAll newM
  where
    (removeCount, newM) = removeOuter m

showMaps :: [(Int, Map)] -> String
showMaps [] = ""
showMaps ((removeCount, m) : xs) = show removeCount ++ "\n" ++ show m ++ "\n" ++ showMaps xs

main :: IO ()
main = do
  input <- lines <$> readFile "d04/input.txt"
  let m = parseLines input
  putStrLn $ "Original Map:\n" ++ show m

  let states = removeAll m

  let mapstr = showMaps states
  putStrLn $ mapstr

  let p1 = fst $ head states
  let p2 = sum $ fst $ unzip states
  putStrLn $ "!!!!!!!!! P1: " ++ show p1
  putStrLn $ "!!!!!!!!! P2: " ++ show p2

-- !!!!!!!!! P1: 1543
-- !!!!!!!!! P2: 9038
