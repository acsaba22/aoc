import Data.List
import Text.Regex.Posix

-- Shift ID | Sleep Minute | Wake Minute
data Event = Shift Int | Sleep Int | Wake Int deriving (Show)

-- [1518-11-23 00:02] Guard #2657 begins shift
-- [1518-11-23 00:22] falls asleep
-- [1518-11-23 00:33] wakes up

datePattern :: String
datePattern = "\\[[0-9]{4}-[0-9]{2}-[0-9]{2} ([0-9]{2}):([0-9]{2})\\] "

parseRe :: String -> String -> [String]
parseRe s patt =
  let (_, match, _, groups) = s =~ patt :: (String, String, String, [String])
  in if match == "" then [] else groups

parseEvent :: String -> Event
parseEvent s =
  -- | not (shift == []) = Shift . read $ shift !! 2
  -- | not (sleep == []) = Sleep . checkhour . read $ sleep !! 1
  -- | not (wake == []) = Wake . checkhour . read $ wake !! 1
  -- | otherwise = error ("Can't parse: " ++ s)
  let
    shift = parseRe s (datePattern ++ "Guard #([0-9]*) begins shift")
    sleep = parseRe s (datePattern ++ "falls asleep")
    wake = parseRe s (datePattern ++ "wakes up")
    checkhour h m = if h == "00" then m else error "wake or sleep should be 0 hour"

    toEvent [h,m,i] [] [] = Shift . read $ shift !! 2
    toEvent [] [h,m] [] = Sleep . read $ checkhour h m
    toEvent [] [] [h,m] = Wake . read $ checkhour h m
    toEvent x y z = error $ "Parse error: " ++ s
  in toEvent shift sleep wake

-- readClaim :: String -> Claim
-- readClaim s =
--   toClaim . parsere $ deleteAll ' ' s
--   where
--     parsere :: String -> [Int]
--     parsere s =
--       -- TODO Misi kerdes: Beszeljunk a regexekrol. Perl style vs Posix? [0-9]-re van-e shorthand (\d)?
--       let (_, match, _, gs) = s =~ "#([0-9]*)@([0-9]*),([0-9]*):([0-9]*)x([0-9]*)$" :: (String, String, String, [String])
--       in if not (match == "") then map (read :: String -> Int) gs else error "Didn't match pattern"
--     toClaim :: [Int] -> Claim
--     toClaim [id, x, y, w, h] = Claim id $ Rect (Interval x (x+w)) (Interval y (y+h))


p1 content = take 10 . map parseEvent . sort . lines $ content

main = do
  content <- readFile "input.txt"
  print $ p1 content
