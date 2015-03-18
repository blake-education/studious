-- ex 1

import Data.List


indexify :: [a] -> [(Integer, a)]
indexify = zip [1..]

unindexify :: [(Integer,a)] -> [a]
unindexify = snd . unzip

wrapFilterUnwrap x = unindexify . filter x . indexify

isDivisibleBy :: Integer -> (Integer, a) -> Bool
isDivisibleBy x (y,_) = mod y x == 0

skips :: [a] -> [[a]]
skips list = [ wrapFilterUnwrap (isDivisibleBy m) list | m <- [1..(fromIntegral $ length list)] ]

-- ex1 golfed

s n = snd . unzip . filter (\(i,_) -> i `mod` n == 0) . zip [1..] 
skips' l = [ s m l | m <- [1..(length l)] ]

{-test = skips' "ABCD"-}


-- ex 2


triples :: [Integer] -> [(Integer,Integer,Integer)]
triples (x1:x2:[]) = []
triples (x1:x2:x3:xs) = [(x1,x2,x3)] ++ triples (x2:x3:xs)

untriple :: (Integer,Integer,Integer) -> Integer
untriple (_,x,_) = x

untriples :: [(Integer,Integer,Integer)] -> [Integer]
untriples = map untriple 

isLocalMax :: (Integer,Integer,Integer) -> Bool
isLocalMax (l,m,r) = m > l && m > r

localMaxima :: [Integer] -> [Integer]
localMaxima = untriples . filter isLocalMax . triples


-- ex2 golfed
t (_:_:[]) = []
t (x:y:z:xs) = [(x,y,z)] ++ t (y:z:xs)

localMaxima' :: [Integer] -> [Integer]
localMaxima' = map (\(_,x,_) -> x) . filter (\(l,m,r) -> m > l && m > r) . t

{-test = localMaxima' [2,9,5,6,1]-}

-- ex 3
histoLine list n = filter (\x -> x == n) list

histo list = map (histoLine list) [0..9]

maxs = maximum . map (length)

starLine max list = (replicate (length list) '*') ++ (replicate (max-(length list)) ' ')

stars list = map (starLine (maxs list)) list

histogram :: [Integer] -> String
histogram list = unlines $ (reverse $ transpose $ stars $ histo list)
  ++ [(replicate 10 '='), ['0'..'9']]

test = histogram [1,4,5,4,6,6,3,4,2,4,9]

main :: IO ()
main = putStr $ test
{-main = print $ test-}
