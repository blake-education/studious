{-# OPTIONS_GHC -Wall #-}

-- CIS 194 Homework 3

module Golf where

-- Exercise 1 : Hopscotch

dr :: Int -> [a] -> [a]
dr _ [] = []
dr n a
  | length a <= n  = []
  | otherwise =
    let rest = drop n a
    in head rest : dr n (tail rest)

multiDrop :: Int -> [a] -> [[a]]
multiDrop n ary
  | n > length(ary) = []
  | otherwise = (dr n ary : multiDrop (n+1) ary)

skips :: [a] -> [[a]]
skips ary = multiDrop 0 ary

-- Exercise 2 : 

maxima :: [Int] -> [Int]
maxima [] = []
maxima (a:b:c:_) = if (b > a) && (b > c) then
                  [b]
                 else
                  []

localMaxima :: [Int] -> [Int]
localMaxima a
  | length a < 3 = []
  | otherwise = maxima (take 3 a) ++ localMaxima (tail a)


-- Exercise 3 :

countMembers :: [Int] -> Int -> Int
countMembers a n = length(filter(==n) a)

star :: Bool -> String
star b = if b then "*" else " "

row :: Int -> [Int] -> String 
row n a = concat (map (star) (map (>=n) a)) ++ "\n"

rows :: Int -> [Int] -> String
rows 0 _ = ""
rows c a = row c a ++ rows (c-1) a

histogram :: [Int] -> String
histogram a =
  rows (maximum counts) counts ++ "==========\n0123456789\n"
  where counts = map (countMembers a) [0..9]



