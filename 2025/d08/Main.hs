module Main (main) where

import Data.List.Extra (sort, split)
-- TODO V.length -et lehet mashogy is importalni?

import Data.Map (Map)
import Data.Map qualified as M
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import GHC.Float (int2Double)

fname :: String
p1N :: Int -- TODO ez furcsan van a feladatban megfogalmazva, -1 kene legyen ugye?

-- fname = "d08/example.txt"
-- p1N = 10

fname = "d08/input.txt"
p1N = 1000

type Point = (Int, Int, Int)

parsePoint :: String -> Point
parsePoint s = case nums of
  [x, y, z] -> (x, y, z)
  _ -> error "3 nums needed"
  where
    nums = map read $ split (== ',') s :: [Int]

dist :: Point -> Point -> Double
dist (x1, y1, z1) (x2, y2, z2) = sqrt sum2
  where
    dx = int2Double $ x2 - x1
    dy = int2Double $ y2 - y1
    dz = int2Double $ z2 - z1
    sum2 = dx ** 2 + dy ** 2 + dz ** 2

type IdxPair = (Int, Int)

type DistList = [(Double, IdxPair)]

type State = Map Int Int

initState :: State
initState = M.empty

-- TODO should I update map when I traverse?
componentId :: State -> Int -> Int
componentId rootMap pid = case M.lookup pid rootMap of
  Just parent -> componentId rootMap parent
  Nothing -> pid

-- TODO same type but diferent menaing (componentid -> size)
type ComponentSizes = Map Int Int

add1 :: Maybe Int -> Maybe Int
add1 (Just x) = Just (x + 1)
add1 Nothing = Just 1

-- TODO igy kell acumulalni, 2x componentsizes?

-- TODO oldSizes, newSizes jo nev?
componentSizes' :: [Int] -> State -> ComponentSizes -> ComponentSizes
componentSizes' [] _ sizes = sizes
componentSizes' (pid : pids) state oldSizes = componentSizes' pids state newSizes
  where
    newSizes = M.alter add1 (componentId state pid) oldSizes

componentSizes :: [Int] -> State -> ComponentSizes
componentSizes pids state = componentSizes' pids state M.empty

type Result = (State, IdxPair)

connect :: [IdxPair] -> State -> [Result]
connect (pair@(p1Idx, p2Idx) : ds) state = ret
  where
    component1 = componentId state p1Idx
    component2 = componentId state p2Idx
    ret =
        -- trace (show (p2Idx, p1Idx)) $
        if component1 == component2
          then connect ds state
          else (newState, pair) : connect ds newState
      where
        small = min component1 component2
        big = max component1 component2
        newState = M.insert big small state
connect [] _ = []

main :: IO ()
main = do
  input <- lines <$> readFile fname
  let points = map parsePoint $ input :: [Point]
  let pVec = V.fromList points :: Vector Point
  let n = length pVec
  let n1 = n - 1
  print pVec
  -- TODO Ez tul hosszu sor, hogy lehet tordelni?
  let dists = [(dist (pVec ! q) (pVec ! w), (q, w)) | q <- [0 .. n1], w <- [q + 1 .. n1]] :: DistList
  -- print dists
  let sortedDists = sort dists
  -- print $ map snd $ sortedDists
  let sortedEdges = map snd sortedDists

  let results1 = connect (take p1N sortedEdges) initState
  -- mapM_ print $ take 5 $ map (map (pVec !))$ map (\(a,b) -> [a, b]) $ results

  -- mapM_ print $ results
  let result1 = fst $ last results1

  print "Result"
  print result1

  let components = componentSizes [0 .. n1] result1
  print components
  let s = reverse $ sort $ map snd $ M.toList components
  print $ s
  print $ sum $ s
  print $ product $ take 3 s

  let (!p1) = product $ take 3 s :: Int

  let results2 = connect sortedEdges initState
  print $ snd $ last results2
  let (p1Idx, p2Idx) = snd $ last results2
  print $ pVec ! p1Idx
  print $ pVec ! p2Idx
  let (x1,_,_) = pVec ! p1Idx
  let (x2,_,_) = pVec ! p2Idx

  -- let final = head lastTwo
  -- let prev = head (tail lastTwo)

  -- let (p1Idx, p2Idx) = head $ M.toList $ M.difference final prev
  -- print $ (p1Idx, p2Idx)

  -- print $ pVec ! p1Idx
  -- print $ pVec ! p2Idx

  let p2 = x1 * x2 :: Int
  putStrLn $ "!!!!!!!!! P1: " ++ show p1
  putStrLn $ "!!!!!!!!! P2: " ++ show p2

-- !!!!!!!!! P1: 96672
