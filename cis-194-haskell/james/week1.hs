import Data.List.Split
import Debug.Trace

toDigits :: Integer -> [Integer]
toDigits number
  {-| trace ("toDigits " ++ show number) False = undefined-}
  | number < 1 = []
  | otherwise = toDigits (div number 10) ++ [mod number 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev number = reverse(toDigits(number))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list
  {-| trace ("doubleEveryOther " ++ show list) False = undefined-}
  | otherwise = (reverse (concatMap doubleSnd (chunksOf 2 (reverse list))))

doubleSnd :: [Integer] -> [Integer]
doubleSnd (x:[]) = x:[]
doubleSnd (x:xs)
  {-| trace ("doubleSnd " ++ show (x:xs)) False = undefined-}
  | otherwise = x:2 * (head xs):tail xs

sumDigits :: [Integer] -> Integer
sumDigits numbers = sum (concatMap toDigits numbers)

validate :: Integer -> Bool
validate number = (mod (sumDigits (doubleEveryOther (toDigits number))) 10) == 0
