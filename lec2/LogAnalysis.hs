{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parse a single line of string to LogMessage
parseMessge :: String -> LogMessage
parseMessge msg = parse' $ words msg
    where parse' ("I":timestamp:remain) = LogMessage Info (read timestamp) (unwords remain)
          parse' ("W":timestamp:remain) = LogMessage Warning (read timestamp) (unwords remain)
          parse' ("E":level:timestamp:remain) = LogMessage (Error (read level)) (read timestamp) (unwords remain)
          parse' _ = Unknown msg

-- parse whole log file to [LogMessage]
parse :: String -> [LogMessage]
parse = map parseMessge . lines

-- get the timestamp from LogMessage
getTimestamp :: LogMessage -> TimeStamp 
getTimestamp (LogMessage _ timestamp _) = timestamp
getTimestamp (Unknown _) = error "no timestamp for unknown"

-- insert LogMessage into MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left node right)
    | getTimestamp msg > getTimestamp node = Node left node (insert msg right)
    | getTimestamp msg < getTimestamp node = Node (insert msg left) node right
    | otherwise = Node left node right

-- build tree from [LogMessage]
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- inOrder will sort the messagetree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++ [node] ++ inOrder right

-- whatWentWrong will extract the error infomation whoes severity >= 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  map extractString . filter go . inOrder . build
    where go (LogMessage (Error l) _ _)
                | l >= 50 = True
                | otherwise = False
          go _ = False
          extractString (LogMessage _ _ s) = s
          extractString _ = error "not support"
