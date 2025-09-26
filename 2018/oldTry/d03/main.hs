import Text.Regex.Posix
import Data.List

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll v = filter (\x -> not (x==v))

-- [from, to)
data Interval = Interval Int Int deriving (Show)

range :: Interval -> [Int]
range (Interval from to) = [from..to-1]

type Point = (Int, Int)

-- TODO Misi kerdes: ezt hogy lehet olyanra csinalni hogy Interval.contains?
intervalContains :: Interval -> Int -> Bool
intervalContains (Interval from to) v = from <= v && v < to

-- xinterval, yinterval
data Rect = Rect Interval Interval deriving (Show)


allPoints :: Rect -> [Point]
allPoints (Rect intx inty) = [(x, y) | x <- range intx , y <- range inty]

rectContains :: Rect -> Point -> Bool
rectContains (Rect intx inty) (x, y) = intx `intervalContains` x && inty `intervalContains` y

-- id rectangle
data Claim = Claim Int Rect deriving (Show)


readClaim :: String -> Claim
readClaim s =
  toClaim . parsere $ deleteAll ' ' s
  where
    parsere :: String -> [Int]
    parsere s =
      -- TODO Misi kerdes: Beszeljunk a regexekrol. Perl style vs Posix? [0-9]-re van-e shorthand (\d)?
      let (_, match, _, gs) = s =~ "#([0-9]*)@([0-9]*),([0-9]*):([0-9]*)x([0-9]*)$" :: (String, String, String, [String])
      in if not (match == "") then map (read :: String -> Int) gs else error "Didn't match pattern"
    toClaim :: [Int] -> Claim
    toClaim [id, x, y, w, h] = Claim id $ Rect (Interval x (x+w)) (Interval y (y+h))


readClaims :: String -> [Claim]
readClaims content = map readClaim $ lines content

-- Points which have multiple claims
badPoints :: [Claim] -> [Point]
badPoints claims =
  let
    groups :: [[Point]]
    groups = group . sort . concat . map (\(Claim _ rect) -> allPoints rect) $ claims
  -- TODO Misi kerdes: ezt nezzuk at igy a jo-e
  in foldr (\x l -> if 1 < length x then head x : l else l  ) [] groups

p1 content =
  let
    claims = readClaims content
  in length . badPoints $ claims

-- exclude claims which include the point
exclude :: Point -> [Claim] -> [Claim]
exclude p claims = foldr (\c@(Claim _ r) l -> if r `rectContains` p then l else c:l) [] claims


excludeAll :: [Point] -> [Claim] -> [Claim]
-- TODO Misi kerdes: beszeljunk foldr/foldl kulonbsegrol itt
-- TODO Misi kerdes: hogy lehet memoriahasznalatot nezni processre
-- excludeAll points claims = foldr (\p cs -> exclude p cs) claims points -- 25s
excludeAll points claims = foldl (\cs p -> exclude p cs) claims points -- 21s

p2 content  =
  let
    claims = readClaims content
    bpoints = badPoints claims
  in excludeAll bpoints claims

main = do
  content <- readFile "input.txt"
  putStr "P1 = "
  print $ p1 content -- 97218
  putStr "P2 = "
  print $ p2 content -- 717
