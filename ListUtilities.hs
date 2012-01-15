module ListUtilities where

replace :: Eq a => a -> a -> [a] -> [a]
replace r w = map (\x -> if x == r then w else x)

padLeft :: Int -> a -> [a] -> [a]
padLeft i x ys = replicate (i - length ys) x ++ ys

padRight :: Int -> a -> [a] -> [a]
padRight i x ys = ys ++ replicate (i - length ys) x
