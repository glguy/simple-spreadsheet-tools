module ListUtilities where

-- | Replace all occurrences of 'r' with 'w' in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace r w = map (\x -> if x == r then w else x)

-- | Add enough 'x' elements to the beginning 'ys' to ensure the list
-- is 'i' elements long. This function will not drop elements.
padLeft :: Int -> a -> [a] -> [a]
padLeft i x ys = replicate (i - length ys) x ++ ys

-- | Add enough 'x' elements to the end 'ys' to ensure the list
-- is 'i' elements long. This function will not drop elements.
padRight :: Int -> a -> [a] -> [a]
padRight i x ys = ys ++ replicate (i - length ys) x
