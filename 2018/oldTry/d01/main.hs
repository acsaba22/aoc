import Debug.Trace

-- p1 :: [Char] -> Int


-- append char to the first string in the list
appendToLines :: Char -> [String] -> [String]
appendToLines c [] = error "There is nothing to append to"
appendToLines c (s : tail) = (c : s) : tail

-- splits into lines, skips trailing empty lines
splitLines :: [Char] -> [String]
splitLines [] = [""]
splitLines ('\n' : tail) =
  let rest = splitLines tail
  in "" : noempty rest
     where noempty [""] = []
           noempty x = x
splitLines (c : tail) = appendToLines c (splitLines tail)


-- toInt_ s ferfixValue -> value
-- s should be digits only
toInt_ :: String -> Int -> Int
toInt_ [] pref = pref
toInt_ (c : s) pref
  | '0' <= c && c <= '9' = toInt_ s ((fromEnum c - fromEnum '0') + 10*pref)
  | otherwise = error ("Unkown charachter" ++ [c])

toInt :: String -> Int
toInt [] = 0
toInt all@(c : s)
  | c == '+' = toInt s
  | c == '-' = -toInt s
  | otherwise = toInt_ all 0


mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs


sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs

toIntList :: String -> [Int]
toIntList s = mapList toInt (splitLines s)

-- ====================================== P1
p1 s = sumInt (toIntList s)

-- If there is less then n returns all list
takeFirst :: Int -> [a] -> [a]
takeFirst 0 xs = []
takeFirst n (x:xs) = x : takeFirst (n-1) xs
takeFirst n [] = []


forever_ :: [a] -> [a] -> [a]
forever_ original [] = forever_ original original
forever_ original (x:xs) = x : forever_ original xs

-- repeat list forever
forever :: [a] -> [a]
forever x = forever_ x x

prefSum_ :: Int-> [Int] -> [Int]
prefSum_ prefVal [] = []
prefSum_ prefVal (x:xs) =
  let val = prefVal+x
  in val : prefSum_ val xs

-- list summs of all prefixes
prefSum :: [Int] -> [Int]
prefSum xs = 0 : prefSum_ 0 xs

inside :: Int -> [Int] -> Bool
inside _ [] = False
inside y (x:xs)
  | x == y = True
  | otherwise = y `inside` xs

-- firstRepeat tail_of_values previously_seen_values -> first_repeated_value
firstRepeat_ :: [Int] -> [Int]-> Int
firstRepeat_ [] prevs = error "No repeats found"
firstRepeat_ (x:xs) prevs =
  (if length xs `mod` 10 == 0 then traceShow (x, length xs) else id) $
  let seen = x `inside` prevs
  in
    if seen
    then x
    else firstRepeat_ xs (x:prevs)

firstRepeat :: [Int] -> Int
firstRepeat xs = firstRepeat_ xs []


-- p2 s = takeFirst 11 (prefSum (forever (toIntList s)))
p2 s = firstRepeat (prefSum (forever (toIntList s)))

-- TODO Misi kerdesek:
-- Ez 1 percig futott mert negyzetes. Ha szekvencialis lenne akkor tok konnyu lenne hozaadni debugolasokat.
-- - [ ] akartam hogy kiirja minden 10000. lepesnel hogy hol tart, hogy lassam hogy halad.
--        debug trace.
-- - [ ] akartam hogy kiirja a legnagyobb es legkissebb szamot amit latott eddig hogy lassam hogy bitset-re hogy mukodne (mekkora min-maxot tegyek)
--        lekodolom inkabb
-- - [ ] array-okrol beszeljunk kicsit

main = do
  content <- readFile "input.txt"
  putStr "P1 = "
  print $ p1 content
  putStr "P2 = "
  print $ p2 content
