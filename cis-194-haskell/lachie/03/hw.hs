-- ex 1

ind :: [a] -> [(Int, a)]
ind = zip [1..]

unind :: [(Int,a)] -> [a]
unind = snd . unzip

wu x = unind . filter x . ind

isDiv x (y,_) = mod y x == 0


skips :: [a] -> [[a]]
skips list = [ wu (isDiv m) list | m <- [1..(length list)] ]

skipper n = snd . unzip . filter (\(i,_) -> i `mod` n == 0) . zip [1..] 

{-test = skipper 0 "ABCD"-}


-- ex 2

triples :: [Integer] -> [(Integer,Integer,Integer)]
triples (x1:x2:[]) = []
triples (x1:x2:x3:xs) = [(x1,x2,x3)] ++ triples (x2:x3:xs)

m (_,x,_) = x

untriples :: [(Integer,Integer,Integer)] -> [Integer]
untriples = map m

localMax (l,m,r) = m > l && m > r

localMaxima :: [Integer] -> [Integer]
localMaxima = untriples . filter localMax . triples

test = localMaxima [2,9,5,6,1]

main :: IO ()
main = print $ test

-- filter (filterMod 2) (ind "ABCD")
