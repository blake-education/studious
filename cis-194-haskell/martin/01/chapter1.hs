sumtorial :: Int -> Int
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

{-hailstoneSeqLength :: Integer -> Int-}
{-hailstoneSeqLength n = length(hailstoneSeq n)-}

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | otherwise = n `mod` 10 : toDigitsRev(n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev n)

isEvenInt :: Int -> Bool
isEvenInt n = n `mod` 2 == 0

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft n
  | length(n) == 1      = n
  | length(n) == 2      = [(head n)] ++ [(last(n) * 2)]
  | otherwise           = doubleEveryOtherLeft(take 2 n) ++ doubleEveryOtherLeft(drop 2 n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherLeft(reverse n))

digitalize :: [Integer] -> [Integer]
digitalize [] = []
digitalize n = toDigits(head n) ++ digitalize(tail(n))

sumDigits :: [Integer] -> Integer
sumDigits n = sum(digitalize(n))

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits(n))) `rem` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi height c1 c2 c3
  | height == 1    = [(c1, c3)]
  | otherwise      = hanoi smaller c1 c3 c2 ++ [(c1, c3)] ++ hanoi smaller c2 c1 c3
  where smaller = (height - 1)

