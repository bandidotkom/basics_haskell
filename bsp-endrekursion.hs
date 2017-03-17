pack :: (Eq a) => [a] -> [(a,Int)]
pack [] = []
pack (x:xs) = packHilf (x,1) xs --Startwert: (erstes Element, Häufigkeit=1), rekursiv weiter mit Hilfsfunktion
		where
			packHilf (y,k) [] = [(y,k)] --wenn leere Liste erreicht, Übergabe des Tupels, k: Zähler für Häufigkeit
			packHilf (y,k) (l:ls)
				| y == l    = packHilf (y,k+1) ls --Element kommt wieder vor, Zähler wird erhöht, rekursiv weiter mit Rest
				| otherwise = (y,k) : packHilf (l,1) ls --Übergabe des Tupels als Listenelement, weiter mit dem nächsten Element

meinPi :: Integer -> Double
meinPi x
	| x<0 = error "falsche Eingabe"
	| otherwise = 4*(quickPi x 1) --x Zaehler, Startwert fuer die Rekursion 1
			where
				quickPi :: Integer -> Double -> Double
				quickPi 0 y = y --Zaehler 0, gibt die berechnete Summe zurück
				quickPi x y =  quickPi (x-1) (y+(((-1)**(fromIntegral x)/(2*(fromIntegral x)+1))))

fib' :: Integer -> Integer
fib' n = quickFib 0 1 n
		where
			quickFib a b 0 = a
			quickFib a b n = quickFib b (a+b) (n-1)

quickRev xs = rev_helper xs []
		where
			rev_helper [] ys = ys
			rev_helper (x:xs) ys = rev_helper xs (x:ys)

lengthEndRek xs = quickl xs 0
		where
			quickl [] n = n
			quickl (x:xs) n = quickl xs (n+1)

lentghFold xs = foldl (\x y -> x+1) 0 xs

sumEndRek :: Integer -> Integer
sumEndRek n = sumHilf n 0
		where
			sumHilf 0 acc = acc
			sumHilf n acc = sumHilf (n-1) (acc+n)

