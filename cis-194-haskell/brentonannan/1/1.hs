toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev i = (i `rem` 10) : toDigitsRev (i `div` 10)

toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0    = []
  | otherwise = reverse (toDigitsRev i)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherFromLeft (reverse l))

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:xys) = x : (2 * y) : doubleEveryOtherFromLeft xys

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x >= 10   = sumDigits (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate i = (sumDigits (doubleEveryOther (toDigits i))) `rem` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a 

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b c d = [(a, b)]



