module Golf where

-- Exercise 1
everyOther :: Int -> [a] -> [a]
everyOther n xs = case drop (n - 1) xs of
                    (y:ys) -> y : everyOther n ys
                    [] -> []

skips :: [a] -> [[a]]
skips [] = []
skips xs = [everyOther n xs | n <- [1..length xs]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:rest@(y:z:xs))
  | y > x && y > z   = y : (localMaxima rest)
  | otherwise        = localMaxima rest
localMaxima _ = []

-- Exercise 3
histogram :: [Integer] -> String
histogram (x:xs) = "*"
