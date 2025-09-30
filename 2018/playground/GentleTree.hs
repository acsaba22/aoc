-- {-# LANGUAGE UndecidableInstances #-}
module Main where

class ShowA a where
    showA :: a -> String

showFunc :: (Num a, Show a) => a -> String
showFunc x = show x

-- TODO(misi): ez miert nem megy?
-- instance (Num a, Show a) => ShowA a where
--     showA x = show x
-- class (Num a, Show a) => NumShow a
-- instance (NumShow a) => ShowA a where
--     showA x = show x

instance ShowA Int where
  showA = show

instance ShowA Integer where
  showA = show

instance ShowA Char where
    showA c = [c]

instance ShowA String where
    showA s = s

instance ShowA a => ShowA [a] where
    showA [] = "[]"
    showA xs = "[" ++ showList xs ++ "]"
      where showList [y] = showA y
            showList (y:ys) = showA y ++ "," ++ showList ys

-- tmp1 = showA "alma"

-- l :: [Int]
-- l = [1,2,3,4,5,6]

-- data Tree a
--   = Leaf a
--   | Branch (Tree a) (Tree a)

-- showTree1 :: (ShowA a) => Tree a -> String
-- showTree1 (Leaf x) = showA x
-- showTree1 (Branch l r) = "<" ++ showTree1 l ++ "," ++ showTree1 r ++ ">"

-- -- instance ShowA a => Tree a where
-- --   showA Leaf x 
-- -- Branch (Branch (Leaf 1 Leaf 2)

-- exampleTree0 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

main :: IO ()
main = do
    putStrLn "Hello from GentleTree!"
    -- putStrLn $ "ShowA Int: " ++ showA (42 :: Int)
    -- putStrLn $ "ShowA Char: " ++ showA 'x'
    -- putStrLn $ "ShowA String: " ++ showA "hello"
    -- putStrLn $ "ShowA [Int]: " ++ showA [1, 2, 3]
