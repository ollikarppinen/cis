module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage x = LogMessage Info 1 x
