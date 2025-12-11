module Main (main) where

import Data.Array (Array, Ix (inRange), bounds, indices, listArray, (!), elems)

type Coor = (Int, Int)


data Map = Map
  { -- TODO replace with size Ix? Inclusive?
    matrix :: Array Coor Char,
    height :: Int,
    width :: Int
  }

valAt :: Map -> Coor -> Char
valAt m c
  | (inRange $ bounds $ matrix m) c = matrix m ! c
  | otherwise = '.' -- TODO make it a constant

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
      otherwise -> '.'
    b = bounds mat
    newMatrix = listArray b $ map newVal (indices mat)
    newMap = Map (newMatrix) h w
    removeCount = sum [1 | c <- elems newMatrix, c=='x']

-- -- TODO lehetseges lenne matrix.indecies-t irni valahogy?
-- countP1 :: Map -> Int
-- countP1 m@(Map mat _ _) = length $ filter (< 4) neighNum
--   where
--     -- sum $ map (\c if c == '@' then 1 else 0) $ map (map (mat !)) $ map neigh8 $ indecies mat

--     -- isPaper :: Char -> toBool
--     -- isPaper = case valueA

--     idxs = filter (\(_, c) -> if c == '@' then True else False) $ assocs mat :: [Coor]
--     neighList = map neigh8 $ indices mat :: [[Coor]]
--     neighVals = traceLabel "neighVals" $ map (map (valAt m)) neighList :: [String]
--     boolList = map (map (\c -> if c == '@' then 1 else 0)) neighVals :: [[Int]]
--     neighNum = traceLabel "neighNum" $ map sum boolList :: [Int]

-- TODO hasznaljak {-# LANGUAGE RecordDotSyntax #-} -ot?
instance Show Map where
  -- can I do show m = "Map{" ++ (show m.height) ++ ", " ++ (show m.width) ++ "}"
  -- show m = "Map{" ++ show (height m) ++ ", " ++ show (width m) ++ "}"
  show m = concat [[(matrix m) ! (y, x) | x <- [0 .. (height m) - 1]] ++ "\n" | y <- [0 .. (width m) - 1]]

parseLines :: [String] -> Map
parseLines ls = Map m h w
  where
    h = length ls
    w = length $ head ls
    m = listArray ((0, 0), (h - 1, w - 1)) $ concat ls

removeAll :: Map -> [(Int, Map)]
removeAll m = case removeCount of
  0 -> []
  otherwise -> (removeCount, newM) : removeAll newM
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
