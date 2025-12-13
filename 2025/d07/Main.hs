module Main (main) where
import qualified Data.ByteString.Char8 as BS

type ByteString = BS.ByteString

indexDefault :: ByteString -> Int -> Char
indexDefault bs i = case maybeVal of 
    Just c -> c
    _ -> '.'
  where
    maybeVal = bs `BS.indexMaybe` i


-- TODO kiszamoltam az uj terkepet, megneztem az aljat, 9 volt de rajottem 
-- hogy az kelett volna hogy hany split volt
-- mas nyelvben olyan konnyu lenne egy kis side data-t kipakolni, itt hogy csinaljam?

propagate :: ByteString -> ByteString -> ByteString
propagate upLine oldState = newState
  where
    newStateS = [newVal i | i <- [0.. (BS.length oldState)-1]]
      where
        newVal i = newChar
          where
            old = oldState `BS.index` i
            up = upLine `BS.index` i
            left = oldState `indexDefault` (i-1)
            leftUp = upLine `indexDefault` (i-1)
            right = oldState `indexDefault` (i+1)
            rightUp = upLine `indexDefault` (i+1)
            newChar = case old of
              '^' -> '^'
              '.' -> if (
                up == 'S' ||
                up == '|' ||
                (left == '^' && leftUp == '|') ||
                (right == '^' && rightUp == '|')) 
                then '|' else '.'
              _ -> error "bad char"
        
    newState = BS.pack newStateS

-- TODO this is like foldl or foldl' but keeps list, probably exists
flowDown :: ByteString -> [ByteString] -> [ByteString]
flowDown upLine (oldState:oldStates) = nextLine : flowDown nextLine oldStates
  where
    nextLine = propagate upLine oldState
flowDown _ [] = []

(!!!) :: BS.ByteString -> Int -> Char
(!!!) = BS.index

splitCount :: (ByteString, ByteString) -> Int
splitCount (up, down) = sum [1 | i <- [0.. (BS.length up) - 1] , isSplit i]
  where
    isSplit i = (up !!! i) == '|' && (down !!! i) == '^'

-- TODO make it a template
-- pairs :: [a] -> [(a,a)]
-- pairs :: [ByteString] -> [(ByteString,ByteString)]
-- pairs (x:y:xs) = (x, y) : (y:xs)

pairs :: [a] -> [(a,a)]
pairs (x:y:xs) = (x, y) : pairs (y:xs)
pairs _ = []

main :: IO ()
main = do
  input <- BS.lines <$> BS.readFile "d07/input.txt"
  mapM_ print input

  print "**************************"

  let p1Field = flowDown (head input) (tail input)
  mapM_ print p1Field

  -- let ps = pairs p1Field :: [(ByteString, ByteString)] 
  -- let s = map splitCount ps :: [Int]
  -- print $ sum $ map splitCount $ pairs p1Field

  -- print $ BS.count '|' $ last p1Field
  let p1 = sum $ map splitCount $ pairs p1Field
  let p2 = 0 :: Int
  putStrLn $ "!!!!!!!!! P1: " ++ show p1
  putStrLn $ "!!!!!!!!! P2: " ++ show p2
