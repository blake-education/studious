{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('E':details) = parseError (words details)
parseMessage ('W':details) = parseDetails Warning (words details)
parseMessage ('I':details) = parseDetails Info (words details)
parseMessage message = Unknown message

parseError :: [String] -> LogMessage
parseError (code:details) = parseDetails (Error (read code)) details

parseDetails :: MessageType -> [String] -> LogMessage
parseDetails mt (ts:text) = LogMessage mt (read ts) (unwords text)

parse :: String -> [LogMessage]
parse messages = map parseMessage (lines messages)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf             = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node left nm@(LogMessage _ nts _) right)
  | ts <= nts = Node (insert lm left) nm right 
  | otherwise = Node left nm (insert lm right)

build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                 = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right) 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = [text | (LogMessage (Error level) _ text) <- lms, level > 50]
