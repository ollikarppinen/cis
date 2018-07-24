module Golf where

nthEl :: [a] -> Int -> [a]
nthEl a n = case drop (n - 1) a of
  []  -> []
  h:t -> h:(nthEl t n)

skips :: [a] -> [[a]]
skips a = map (nthEl a) [1..length a]
