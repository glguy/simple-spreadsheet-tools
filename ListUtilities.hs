module ListUtilities where

replace r w = map (\x -> if x == r then w else x)

padLeft i x ys = replicate (i - length ys) x ++ ys

padRight i x ys = ys ++ replicate (i - length ys) x
