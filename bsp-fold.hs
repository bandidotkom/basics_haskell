mapFold f xs   = foldr (\x xs -> (f x):xs) [] xs
mapFold' f xs = foldr ((:) . f) [] xs

filterFold p xs = foldr (\x xs -> if p x then x:xs else xs ) [] xs

sumList xs = foldr (+) 0 xs
sumList' xs = foldl (+) 0 xs

faktFold n = foldl (*) 1 [1..n]
faktFold' n = foldl (*) 1 [1..n]

powFold x n = foldr (*) 1 (take n [x,x..x])
powFold' x n = foldl (*) 1 (take n [x,x..x])

maxi (x:xs) = foldr max x xs
maxi' (x:xs) = foldl max x xs

reverseFold xs = foldl (flip (:)) [] xs

skalarProd :: [Integer] -> [Integer] -> Integer
skalarProd xs ys = foldl (+) 0 (zipWith (*) xs ys)

isortFold :: [Integer] -> [Integer]
isortFold xs = foldr (ins) [] xs
		where
			ins :: Ord a => a -> -[a] -> [a]
			ins x [] = [x]
			ins x (y:ys)
					| x<= y = x:(y:ys)
					| otherwise = y:(ins x xs)

