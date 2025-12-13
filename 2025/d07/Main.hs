module Main (main) where
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V -- TODO V.length -et lehet mashogy is importalni?

fname :: String
fname = "d07/example.txt"

type CI = (Char, Int)

toCI :: Char -> CI
toCI 'S' = ('|', 1)
toCI c = (c, 0)

type Vec = Vector CI

toPrint :: Vec -> String
toPrint v = map fst $ V.toList v

toPrintAll :: [Vec] -> String
toPrintAll vv = unlines $ map toPrint vv

indexDefault :: Vec -> Int -> CI
indexDefault bs i = case maybeVal of 
    Just c -> c
    _ -> ('.', 0)
  where
    maybeVal = bs !? i


-- TODO kiszamoltam az uj terkepet, megneztem az aljat, 9 volt de rajottem 
-- hogy az kelett volna hogy hany split volt
-- mas nyelvben olyan konnyu lenne egy kis side data-t kipakolni, itt hogy csinaljam?

propagate :: Vec -> Vec -> Vec
propagate upLine oldState = newState
  where
    newStateS = [newVal i | i <- [0.. (V.length oldState)-1]]
      where
        newVal i = newChar
          where
            old = oldState ! i
            up = upLine ! i
            left = oldState `indexDefault` (i-1)
            leftUp = upLine `indexDefault` (i-1)
            right = oldState `indexDefault` (i+1)
            rightUp = upLine `indexDefault` (i+1)
            timelines = (
              snd up +
              (if fst left == '^' then snd leftUp else 0) +
              (if fst right == '^' then snd rightUp else 0))
            
            newChar = case fst old of
              '^' -> ('^', 0)
              '.' -> ((if timelines == 0 then '.' else '|'), timelines)
              _ -> error "bad char"
        
    newState = V.fromList newStateS

-- TODO this is like foldl or foldl' but keeps list, probably exists
flowDown :: Vec -> [Vec] -> [Vec]
flowDown upLine (oldState:oldStates) = nextLine : flowDown nextLine oldStates
  where
    nextLine = propagate upLine oldState
flowDown _ [] = []

splitCount :: (Vec, Vec) -> Int
splitCount (up, down) = sum [1 | i <- [0.. (V.length up) - 1] , isSplit i]
  where
    isSplit i = fst (up ! i) == '|' && fst (down ! i) == '^'

pairs :: [a] -> [(a,a)]
pairs (x:y:xs) = (x, y) : pairs (y:xs)
pairs _ = []

main :: IO ()
main = do
  input <- lines <$> readFile fname
  let inputV = map V.fromList $ map (map toCI) input :: [Vec]
  putStrLn $ toPrintAll inputV

  putStrLn "**************************"

  let p1Field = flowDown (head inputV) (tail inputV)
  putStrLn $ toPrintAll p1Field

  let p1 = sum $ map splitCount $ pairs p1Field
  let p2 = sum $ V.map snd $ last p1Field :: Int
  putStrLn $ "!!!!!!!!! P1: " ++ show p1
  putStrLn $ "!!!!!!!!! P2: " ++ show p2
