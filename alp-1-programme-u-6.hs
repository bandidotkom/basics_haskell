--Aufgabe 3
selectSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
selectSort f [] = error "Kein nicht"
selectSort f [x] = [x]
selectSort f (x:xs) = (getFirst f (x:xs)):(selectSort f (delFirst (getFirst f (x:xs)) (x:xs)))
 
getFirst :: (Ord a) => (a -> a -> Bool) -> [a] -> a
getFirst f [x] = x
getFirst f (x:xs)
    |  f x (getFirst f xs) = x    
    | otherwise = getFirst f xs
 
delFirst :: (Eq a) => a -> [a] -> [a]
delFirst x [] = []
delFirst x (y:ys)
    | (x == y) = ys
    | otherwise = y:(delFirst x ys)