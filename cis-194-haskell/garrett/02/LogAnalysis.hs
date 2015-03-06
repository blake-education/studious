{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

-- Exercise 1
parseTimeStamp :: String -> TimeStamp
parseTimeStamp time = read time :: Int

parseString :: [String] -> String
parseString str = unwords str

parseError :: String -> MessageType
parseError str = Error(read str :: Int)

parseMessage :: String -> LogMessage
parseMessage str = case words str of
                    ("I":t:s)   -> LogMessage Info (parseTimeStamp(t)) (parseString(s))
                    ("W":t:s)   -> LogMessage Warning (parseTimeStamp(t)) (parseString(s))
                    ("E":i:t:s) -> LogMessage (parseError(i)) (parseTimeStamp(t)) (parseString(s))
                    _           -> Unknown str

parse :: String -> [LogMessage]
parse str = map parseMessage (lines(str))


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ timeStamp _) (Node leftTree treeLM@(LogMessage _ treeTimeStamp _) rightTree)
  | timeStamp < treeTimeStamp = Node (insert lm leftTree) treeLM rightTree
  | otherwise                 = Node leftTree treeLM (insert lm rightTree)


-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) = (inOrder leftTree) ++ (logMessage : (inOrder rightTree))


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x:xs) = case x of
                        LogMessage (Error n) _ msg
                            | n >= 50   -> msg : whatWentWrong xs
                            | otherwise -> whatWentWrong xs
                        _               -> whatWentWrong xs
