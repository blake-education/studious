-- ex 1a
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' = foldl (\rest x -> (x-2) * rest) 1 . filter (even)
{-test = fun1' [4,8,1]-}

-- ex 1b
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


test = fun2 10

main :: IO ()
main = print $ test
