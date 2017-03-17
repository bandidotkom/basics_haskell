--Aufgabe 1
balanced:: [Char] -> Bool
balanced text = bal [] text
			where
				bal :: [Char] -> [Char] -> Bool
				bal [] [] = True
				bal stapel ('(':xs) = bal ('(':stapel) xs
				bal stapel ('[':xs) = bal ('[':stapel) xs
				bal stapel ('{':xs) = bal ('{':stapel) xs
				bal ('(':stapel) (')':xs) = bal stapel xs
				bal ('[':stapel) (']':xs) = bal stapel xs
				bal ('{':stapel) ('}':xs) = bal stapel xs
				bal _ _ = False

balanced2:: [Char] -> Bool --kontrolliert die Klammersetzung in einer beliebigen Zeichenkette
balanced2 text = bal2 [] text
			where
				bal2:: [Char] -> [Char] -> Bool
				bal2 [] [] = True
				bal2 stapel ('(':xs) = bal2 (')':stapel) xs
				bal2 stapel ('[':xs) = bal2 (']':stapel) xs
				bal2 stapel ('{':xs) = bal2 ('}':stapel) xs
				bal2 (s:stapel) (x:xs) | s==x = bal2 stapel xs
				bal2 _ _ = False

--Aufgabe2
random :: Int -> Int
random seed = (25173*seed + 13849) `mod` 65536
randList :: Int -> [Int]
randList n = randList' n [random n]
		where
			randList' n xs
				| (length xs) == n = xs
				| otherwise = randList' n (a:xs)
						where
							a = random (head xs)

randUntilRepeat :: Int -> [Int]
randUntilRepeat seed = rUR seed [random seed]
				where
					rUR seed xs | a elem xs = xs
								| otherwise = rUR seed (a:xs)
									where
										a = random (head xs)