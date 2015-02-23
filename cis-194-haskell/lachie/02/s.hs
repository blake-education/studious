{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: [String] -> LogMessage
parseMessage ("I":(ts:msg)) = LogMessage Info (read ts) (unwords msg)
parseMessage ("W":(ts:msg)) = LogMessage Warning (read ts) (unwords msg)
parseMessage ("E":(errno:(ts:msg))) = LogMessage (Error $ read errno) (read ts) (unwords msg)
parseMessage x = Unknown (unwords x)


parse :: String -> [LogMessage]
parse s = map parseMessage . map words $ lines s

ltMessage :: LogMessage -> LogMessage -> Bool
ltMessage (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 < ts2
ltMessage _ _ = False

gtTree :: LogMessage -> MessageTree -> MessageTree
gtTree msg Leaf = Node Leaf msg Leaf
gtTree (Unknown _) tree = tree
gtTree msg (Node left treeMsg Leaf) = 
gtTree msg (Node Leaf treeMsg right) = 
gtTree msg (Node left treeMsg right) | (ltMessage msg treeMsg) = (gtTree msg (Node left treeMsg right))
                                     | otherwise = (gtTree msg (Node left treeMsg right))

{-ltTree :: LogMessage -> MessageTree -> MessageTree-}
{-ltTree msg Leaf = Leaf-}
{-ltTree (Unknown _) tree = tree-}
{-ltTree msg@(LogMessage _ ts _) tree = Leaf-}

{-insert :: LogMessage -> MessageTree -> MessageTree-}
{-insert (Unknown _) tree = tree-}
{-insert msg Leaf = Node Leaf msg Leaf-}
{-insert msg tree = Node (ltTree msg tree) msg (gtTree msg tree)-}


-- main = testParse parser 10 "sample.log"

{-main :: IO [LogMessage]-}
{-main = testParse parse 20 "sample.log"-}
main :: IO ()
main = print $ ltMessage (LogMessage Info 12 "hi") (LogMessage Info 11 "hi")
