--Aufgabe 3
binAdd :: [Int] -> [Int] -> [Int]
binAdd xs ys | length xs /= 8 || length ys /= 8 = error "falsche Eingabe"
			| otherwise = doAdd [] xs ys
				where
					doAdd xs [] _ = xs
					doAdd erg (x:xs) (y:ys) = doAdd (div (x+y) 2) erg++[(mod (x+y) 2)] xs ys

binSub :: [Int] -> [Int] -> [Int]
binSub xs ys = binAdd xs zweierKomp ys

zweierKomp :: [Int] -> [Int]
zweierKomp xs = binAdd (inv xs) [1,0,0,0,0,0,0,0]
			where
				inv [] = []
				inv (0:xs) = 1:inv(xs)
				inv (1:xs) = 0:inv(xs)
--Aufgabe 4
skipLetters :: [Char] -> [Char]
skipLetters [] = []
skipLetters (x:xs) | x>='A' && x<='Z' || x>='a' && x<='z' = skipLetters xs
					| otherwise = x: skipLetters xs		
					
skipLetters2 :: [Char] -> [Char]
skipLetters2 [] = []
skipLetters2 (x:xs) | elem x ['a' .. 'z'] || elem x ['A' .. 'Z'] = skipLetters2 xs
					| otherwise = x: skipLetters2 xs



--Aufgabe 5
applyToFirst :: (a -> b) -> (a -> Bool) -> [a] -> b
applyToFirst f p []	= error "nicht gefunden"
applyToFirst f p (x:xs) | p x = f x
						| otherwise = applyToFirst f p xs

applyToAll :: (a->b) -> a->Bool -> [a] -> [b]
applyToAll _ _ [] = []
applyToAll f p (x:xs) | p x = [f x]++(applyToAll f p xs)
						| otherwise = applyToAll f p xs