{-# LANGUAGE TupleSections #-}

module Golf where

import Data.List
import Data.Maybe


-- Exercise 1

skips :: [a] -> [[a]]
skips xs =
  map (\n -> nth n xs) ns
  where
        ns = [1..length xs]
        nth n = (map snd) . (filter (\x -> fst x `rem` n == 0)) . (zip ns)

--  dr 2 "hello!"
dr m = unfoldr ((\x-> fmap (,drop m x) (listToMaybe x)))

-- nth2 n = (map snd) . (filter (\x -> fst x `rem` n == 0)) . (zip ns)
--      where ns = [1..length ns]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:x@(b:c:cs))
            | b > a && b > c = b:(localMaxima x)
            | otherwise = localMaxima x
localMaxima o = []

-- Exercise 3

histogram :: [Int] -> String
histogram xs = toStr $ foldl (\a n -> incr n a) (map (const 0) [1..10]) xs

toStr xs = unlines $ tr $ map (\(i, n) ->  show i ++ "=" ++ replicate n '*') $ zip [0..9] xs

incr n acc = take n acc ++ [(acc !! n) + 1] ++ drop (n+1) acc

col n xs
      | xs == [] = []
      | n == 0   = []
      | otherwise = (map head xs) : (col (n-1) $ map tail xs)

tr xs =
  col maxLen $ map (pad) xs
  where
      maxLen = maximum . map length $ xs
      pad x = (replicate (maxLen - length x) ' ') ++ reverse x
