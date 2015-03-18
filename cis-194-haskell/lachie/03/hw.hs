-- ex 1

import Data.List

-- turn a list into pairs of (index, value)
indexify :: [a] -> [(Integer, a)]
indexify = zip [1..]

-- inverse of indexify
unindexify :: [(Integer,a)] -> [a]
unindexify = snd . unzip

-- wrap in working structure -> filter using working structure -> unwrap
wrapFilterUnwrap x = unindexify . filter x . indexify

-- is the index in the pair divisible by x?
isDivisibleBy :: Integer -> (Integer, a) -> Bool
isDivisibleBy x (y,_) = mod y x == 0

-- for each number 1 to length of the list (m), filter the list using m
skips :: [a] -> [[a]]
skips list = [ wrapFilterUnwrap (isDivisibleBy m) list | m <- [1..(fromIntegral $ length list)] ]

-- ex1 golfed

-- zip into an (index, value) pair -> filter pairs by divisibility of the index -> unzip and take the values
s n = snd . unzip . filter (\(i,_) -> i `mod` n == 0) . zip [1..] 

-- for each number 1 to length of the list (m), filter the list using m
skips' l = [ s m l | m <- [1..(length l)] ]

{-test = skips' "ABCD"-}


-- ex 2

type Triple = (Integer, Integer, Integer)

-- convert a list into overlapping triples
triples :: [Integer] -> [(Integer,Integer,Integer)]
triples (x1:x2:[]) = []
triples (x1:x2:x3:xs) = [(x1,x2,x3)] ++ triples (x2:x3:xs)

-- get the middle value from the triple
untriple :: (Integer,Integer,Integer) -> Integer
untriple (_,x,_) = x

-- inverse of triples
untriples :: [(Integer,Integer,Integer)] -> [Integer]
untriples = map untriple 

-- is a triple a local max?
isLocalMax :: (Integer,Integer,Integer) -> Bool
isLocalMax (l,m,r) = m > l && m > r

-- wrap in working structure ((Integer,Integer,Integer)) -> filter using working structure -> unwrap for value
localMaxima :: [Integer] -> [Integer]
localMaxima = untriples . filter isLocalMax . triples


-- ex2 golfed
t (_:_:[]) = []
t (x:y:z:xs) = [(x,y,z)] ++ t (y:z:xs)

localMaxima' :: [Integer] -> [Integer]
localMaxima' = map (\(_,x,_) -> x) . filter (\(l,m,r) -> m > l && m > r) . t

{-test = localMaxima' [2,9,5,6,1]-}

-- ex 3
-- filter list for numbers == n
histoLine list n = filter (\x -> x == n) list

-- create lists of lists matching each digit 0..9
histoLines list = map (histoLine list) [0..9]

-- find the length of the longest list
maxLength = maximum . map (length)

-- replace numbers with '*' and pad out to max with spaces.
starLine max list = (replicate (length list) '*') ++ (replicate (max-(length list)) ' ')

-- make a starLine for each histoLine
replaceWithStars histoLines = map (starLine (maxLength histoLines)) histoLines

-- output the graph
-- histo list -> replace with stars -> transpose -> reverse -> join into a string with unlines
-- append ='s & number list
histogram :: [Integer] -> String
histogram list = unlines $ (reverse $ transpose $ replaceWithStars $ histoLines list)
  ++ [(replicate 10 '='), ['0'..'9']]

-- test = histogram [1,4,5,4,6,6,3,4,2,4,9]
test = histogram [1,4,1,5,3,7,5,9,5,6,3,2,5,9,3,9,7,7,8,5]

main :: IO ()
main = putStr $ test
{-main = print $ test-}
