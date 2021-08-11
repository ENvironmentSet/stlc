module Find where

find :: [a] -> Int -> Maybe a
find (x:_) 0 = Just x
find (_:xs) i = find xs (i - 1)
find [] _ = Nothing