module Week4 where

import Data.List

-- * every thing in the array
-- if even, x - 2
-- if odd, leave x alone
-- if empty array return 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- sum if it's even while it's not == 1 iterating over n div 2 if even or 3*n+1 if odd
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate f
        where f n = if even n then n `div` 2 else 3 * n + 1

-- data Tree a = Leaf
--             | Node Integer (Tree a) a (Tree a)
--             deriving (Show, Eq)

-- foldTree :: [a] -> Tree a
-- foldTree = ...


xor :: [Bool] -> Bool
xor = odd . length . filter (==True)

-- don't understand this.
-- filter (id) will filter a list of [False, True] to just [True]
-- foldr (list inital -> not inital || not list) False <- confusing
xor' :: [Bool] -> Bool
xor' = foldr (\list inital -> not inital || not list) False. filter (id)


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\list inital -> (f list) : inital) []
