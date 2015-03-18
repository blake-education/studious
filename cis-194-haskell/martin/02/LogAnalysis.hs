{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

{-EXERCISE 1-}

isValidMessageType :: String -> Bool
isValidMessageType s = case head s of 'I' -> True
                                      'W' -> True
                                      'E' -> True
                                      _ -> False

isErrorMessage :: String -> Bool
isErrorMessage str
  | head(str) == 'E' = True
  | otherwise = False

getMessageType :: String -> MessageType
getMessageType s = case head s of 'I' -> Info
                                  'W' -> Warning
                                  'E' -> Error(read(head(tail(words s))))

createMessage :: MessageType -> [String] -> LogMessage
createMessage mt parts =
  let desc = tail(parts)
      ts = read(head parts)
  in LogMessage mt ts (concat desc)


parseMessage :: [Char] -> LogMessage
parseMessage msg =
  if isValidMessageType msg then
    let mt = getMessageType msg
        parts = words msg 
    in 
      if isErrorMessage msg then
        createMessage mt (tail (tail parts))
      else
        createMessage mt (tail parts)
  else
    Unknown msg

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines s
  | length(s) == 1    = [parseMessage (head s)]
  | otherwise         = parseMessage (head s) : parseLines (tail s)

parse :: String -> [LogMessage]
parse s = parseLines (lines s)

{-EXERCISE 2-}

isUnknown :: LogMessage -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False

insert :: LogMessage -> MessageTree -> MessageTree
insert lm mt
  | isUnknown(lm) = mt
  | insertIntoTree lm mt
