{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = mod x 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits x | x < 0     = []
              | otherwise =  (lastDigit x) : toRevDigits (dropLastDigit x)


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:xs)) = x : y * 2 : doubleEveryOther xs

-- doubleEveryOther list = (head list) : (head (tail list)) * 2 : doubleEveryOther (tail (tail list))

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.


sumOne :: Integer -> Integer
sumOne x = foldl (+) 0 (toRevDigits x)

sumDigits :: [Integer] -> Integer
sumDigits x = foldl (+) 0 (map sumOne x)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = lastDigit (sumDigits (doubleEveryOther (toRevDigits x))) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n-1) a b c

-- move n−1 discs from A to B. This leaves disc n alone on peg A
-- move disc n from A to C
-- move n−1 discs from B to C so they sit on disc n
