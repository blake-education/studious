{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: [String] -> LogMessage
parseMessage ("I":(ts:msg)) = LogMessage Info (read ts) (unwords msg)
parseMessage ("W":(ts:msg)) = LogMessage Warning (read ts) (unwords msg)
parseMessage ("E":(errno:(ts:msg))) = LogMessage (Error $ read errno) (read ts) (unwords msg)
parseMessage x = Unknown (unwords x)

parse :: String -> [LogMessage]
parse = map parseMessage . map words . lines

ltMessage :: LogMessage -> LogMessage -> Bool
ltMessage (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 < ts2
ltMessage _ _ = False

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree
insert msg (Node left treeMsg right) | (ltMessage msg treeMsg) = Node (insert msg left) treeMsg right
                                     | otherwise = Node left treeMsg (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isBadMessage :: LogMessage -> Bool
isBadMessage (LogMessage (Error severity) _ _) = severity >= 50
isBadMessage _ = False

messageStrings :: [LogMessage] -> [String]
messageStrings = map ( \(LogMessage _ _ msg) -> msg )

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = messageStrings . filter isBadMessage . inOrder . build

main :: IO [String]
main = testWhatWentWrong parse whatWentWrong "error.log"
