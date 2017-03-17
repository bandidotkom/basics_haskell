--Aufgabe 1
genSuffixes :: [Char] -> [[Char]]
genSuffixes [] = []
genSuffixes (x:xs) = (x:xs):genSuffixes xs

--Aufgabe 2
prefix :: [Char] -> [Char] -> [Char]
prefix [] _ = []
prefix _ [] = []
prefix (x:xs) (y:ys) | x==y = x : prefix xs ys
					| otherwise = []

--Aufgabe 3
longestPrefix :: [[Char]] -> (Int, [Char])
longestPrefix xs = findPrefix xs (0,"")
	where
		findPrefix :: [[Char]] -> (Int, [Char]) -> (Int, [Char])
		findPrefix [] tpl = tpl
		findPrefix [x] tpl = tpl
		findPrefix (x:y:xs) (n, pfx) | length (prefix x y) > n = findPrefix (y:xs) (length (prefix x y), prefix x y)
									| otherwise = findPrefix (y:xs) (n, pfx)

--Probeklausur: erstes Vorkommen von a in c wird durch b ersetzt
replace :: String -> String -> String -> String
replace xs ys [] = []
replace xs ys (z:zs) 
				| prefix xs (z:zs) == xs = ys ++ (delP xs (z:zs))
				| otherwise = z:(replace xs ys zs)
					where
						delP [] ws = ws
						delP (v:vs) (w:ws) = delP vs ws

--laengste Zeichenkette in einem Text
lrs_Algorithm :: (Ord a) => [a] -> [a]
lrs_Algorithm xs = snd.longestPrefix.startMergeSort.genSuff xs --mit Komposition



--anders:
lrs1 :: String -> String
lrs1 xs = reverse (snd(longestPrefix(mySort(filter f (genSuffixes (reverse xs))))))
	where
		f :: String -> String
		f ('C':'A':'T':_) = True
		f _ = False