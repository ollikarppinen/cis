module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message = case words message of
  "I":t:txt   -> LogMessage Info                  (read t::Int) (unwords txt)
  "W":t:txt   -> LogMessage Warning               (read t::Int) (unwords txt)
  "E":s:t:txt -> LogMessage (Error (read s::Int)) (read t::Int) (unwords txt)
  txt         -> Unknown (unwords txt)

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

getTs :: LogMessage -> Int
getTs (LogMessage _ ts _) = ts

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown message) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message (Node lt nm rt) = lt

