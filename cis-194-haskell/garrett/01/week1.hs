module Week1 where


toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (toDigits $ n `div` 10) ++ (toDigits $ n `rem` 10)
  -- `$` is a quicker way instead of wrapping in another ()'s
  -- 123 `div` 10 => 12
  -- 123 `rem` 10 => 3
  -- call this recursively to keep splitting the first + last and combining them together into 1 list


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (toDigitsRev $ n `rem` 10) ++ (toDigitsRev $ n `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (h:[]) = [h]
doubleEveryOther (x:y:zs)
  -- if there is odd number of items, double the middle one => send the rest back through
  | (length (x:y:zs)) `mod` 2 /= 0      = x : y*2 : doubleEveryOther zs
  -- if there is even number of items, double the first one => send the rest back through
  | otherwise                           = x*2 : y : doubleEveryOther zs


sumDigits :: [Integer] -> Integer
sumDigits (h:[]) = h
sumDigits (h:t) = sumDigits(toDigits(h)) + sumDigits(t)


--Double the value of every second digit beginning from the right.
--  That is, the last digit is unchanged; the second-to-last digit is doubled;
--  the third-to-last digit is unchanged; and so on.
--  For example, [1,3,8,6] becomes [2,3,16,6].

--Add the digits of the doubled values and the undoubled digits from the original number.
--  For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.

--Calculate the remainder when the sum is divided by 10.
--  For the above example, the remainder would be 8.

--If the result equals 0, then the number is valid.
validate :: Integer -> Bool
validate n = sumDigits(toDigitsRev(n)) `rem` 10 /= 0


--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)
