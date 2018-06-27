module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message = case words message of
  "I":t:txt   -> LogMessage Info                  (read t::Int) (unwords txt)
  "W":t:txt   -> LogMessage Warning               (read t::Int) (unwords txt)
  "E":s:t:txt -> LogMessage (Error (read s::Int)) (read t::Int) (unwords txt)
  txt         -> Unknown (unwords txt)

