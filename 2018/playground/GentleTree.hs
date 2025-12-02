-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE UndecidableInstances #-}
module GentleTree where
import Data.Bits (Bits(xor))
import Data.Char (ord)

class ShowA a where
    showA :: a -> String
    showListA :: [a] -> String
    showListA [] = "[]"
    showListA xs = "[" ++ showListHelper xs ++ "]"
      where showListHelper [y] = showA y
            showListHelper (y:ys) = showA y ++ "," ++ showListHelper ys

showFunc :: (Num a, Show a) => a -> String
showFunc x = show x

-- -- TODO(misi): ez miert nem megy? a
-- instance (Num a, Show a) => ShowA a where
--     showA x = show x

-- class (Num a, Show a) => NumShow a
-- instance (NumShow a) => ShowA a where
--     showA x = show x

instance ShowA Int where
  showA :: Int -> String
  showA = show

instance ShowA Integer where
  showA = show

instance ShowA Char where
    showA c = [c]
    showListA s = s

instance ShowA a => ShowA [a] where
    showA xs = showListA xs

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

showTree1 :: (ShowA a) => Tree a -> String
showTree1 (Leaf x) = showA x
-- TODO(misi): ezzel nagyon lassu... ++ linearis a bal oldallal? meg tudjuk
-- csinalni a sajat ++ verzionkat hogy gyorsabb legyen?
showTree1 (Branch l r) = "<" ++ showTree1 l ++ "," ++ showTree1 r ++ ">"

-- -- instance ShowA a => Tree a where
-- --   showA Leaf x
-- -- Branch (Branch (Leaf 1 Leaf 2)

exampleTree0 :: Tree Int
exampleTree0 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

createPow2Tree :: Int -> Tree Int
createPow2Tree n = createFrom 0 n
  where
    createFrom :: Int -> Int -> Tree Int
    createFrom k 0 = Leaf k
    createFrom k n = Branch (createFrom k (n-1)) (createFrom (k+2^(n-1)) (n-1))

outStrNK n k = take n (showTree1 (createPow2Tree k))
outStr = outStrNK 10000000 32

outTree1 = createPow2Tree 20
outStr1 = showTree1 outTree1
-- instance NFData a => NFData (Tree a) where
--   rnf (Leaf x) = rnf x
--   rnf (Branch l r) = rnf l `seq` rnf r
-- outTreeEvaluated = outTree `deepseq` outTree
-- outStr2 = showTree1 outTreeEvaluated

writeToFile :: String -> IO ()
writeToFile content = do
    writeFile "treeOut.txt" content
    putStrLn $ "Written " ++ show (length content) ++ " characters to treeOut.txt"

xorString :: String -> Int
xorString = foldl (\acc c -> acc `xor` ord c) 0

showsTree2 :: (Show a) => Tree a -> String -> String
showsTree2 (Leaf x) s = shows x s
showsTree2 (Branch l r) s = '<' : showsTree2 l (',' : (showsTree2 r ('>' : s)))

showTree2 t = showsTree2 t ""

outTree2 = createPow2Tree 20
outStr2 = showTree2 outTree2

slowList :: Int -> [Int]
slowList 0 = []
slowList n = slowList (n - 1) ++ [n]

fastList :: Int -> [Int]
fastList n = fastListHelper n []
  where
    fastListHelper 0 acc = acc
    fastListHelper n acc = fastListHelper (n - 1) (n : acc)

-- Misi megoldasa: $!-el ki is kell ertekelni, hogy ne maradjon k+1
-- azt mondta hogy ha nem length-et hanem sum-ot csinalunk akkor gyors es igaza volt.
fastSmallList :: Int -> [Int]
fastSmallList n = fastSmallListHelper 1 n
  where
    fastSmallListHelper _ 0 = []
    -- fastSmallListHelper k n = k : (fastSmallListHelper (k+1)) (n-1)
    fastSmallListHelper k n = k : (fastSmallListHelper $! (k+1)) (n-1)

treeMain :: IO ()
treeMain = do
    putStrLn "Hello from GentleTree!"
    -- putStrLn $ "outStrLen: " ++ show (length outStr1)
    -- putStrLn $ "outStrLen: " ++ show (length outStr2)
    -- putStrLn $ "outStrLen: " ++ show (length (take (10^7) (fastSmallList (10^8))))
    putStrLn $ "outStrLen: " ++ show (length (take (10^7) (fastSmallList (10^8))))
    putStrLn $ "Tree: " ++ showTree2 (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3))

    -- writeToFile outStr
    -- putStrLn $ "ShowA Int: " ++ showA (42 :: Int)
    -- putStrLn $ "ShowA Char: " ++ showA 'x'
    -- putStrLn $ "ShowA String: " ++ showA "hello"
    -- putStrLn $ "ShowA [Int]: " ++ showA [1, 2, 3]
