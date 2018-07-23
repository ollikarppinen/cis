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
insert (LogMessage t1 ts1 txt1) (Node lt (LogMessage t2 ts2 txt2) rt) = if ts1 < ts2
  then (Node (insert (LogMessage t1 ts1 txt1) lt) (LogMessage t2 ts2 txt2) rt)
  else (Node lt (LogMessage t2 ts2 txt2) (insert (LogMessage t1 ts1 txt1) rt))

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (hd:tl) = insert hd (build tl)

