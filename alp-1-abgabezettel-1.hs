--ProInfo 2 Funktionale Programmierung SoSe 2013
--Abgabezettel 1, 15.07.2012
--Andras Pal Komaromy
--Tutor: David Bohn

--Aufgabe 1
rgb2cmyk :: (Int, Int, Int) -> (Double, Double, Double, Double)
rgb2cmyk (r, g, b)
	|r<0 || r>255 || g<0 || g>255 || b<0 || b>255 = error "falsche Eingabe"
	|(r,g,b) == (0,0,0) = (0.0, 0.0, 0.0, 0.1)
	|otherwise = (c,m,y,k)
				where
					w = max ((fromIntegral r)/255) (max ((fromIntegral g)/255) ((fromIntegral b)/255))
					c = ((w - ((fromIntegral r)/255))/w)
					m = ((w - ((fromIntegral g)/255))/w)
					y = ((w - ((fromIntegral b)/255))/w)
					k = (1 - w)
--Aufgabe 2
maxInt :: [Integer] -> Integer
maxInt [] = error "Maximum der leeren Liste existiert nicht"
maxInt [x] = x
maxInt (x:xs) 
			| x >= maxRest xs = x
			| otherwise = maxRest xs
				where
					maxRest :: [Integer] -> Integer
					maxRest xs = maxInt xs

--Aufgabe 3
mersenneZahlen :: Integer -> [Integer]
mersenneZahlen x = [(2^x)-1 | x <- [0..x] ]

--Aufgabe 4
meinPi :: Integer -> Double
meinPi x
	| x<0 = error "falsche Eingabe"
	| otherwise = 4*(quickPi x 1) --x Zaehler, Startwert fuer die Rekursion 1
			where
			quickPi :: Integer -> Double -> Double
			quickPi 0 y = y --Zaehler 0, gibt die berechnete Summe zurück
			quickPi x y =  quickPi (x-1) (y+(((-1)**(fromIntegral x)/(2*(fromIntegral x)+1))))

--Aufgabe 5
coins :: [Integer]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

change :: Integer -> [Integer]
change m = changeHilf m coins
		where
			changeHilf :: Integer -> [Integer] -> [Integer]
			changeHilf _ [] = []
			changeHilf n (x:xs) = div n x : changeHilf (mod n x) xs --ganzzahlige Division durch die jeweils größte Münze, dann rekursiv weiter mit dem Rest und der nächstgrößeren Münze  

--Aufgabe 6
primzahlen :: Integer -> [Integer] --Funktion aus der Vorlesung: Sieb von Erathostenes
primzahlen n = sieb [2..n]
		where
			sieb [] = []
			sieb (p:xs) = p: sieb [k | k<-xs, k `mod` p>0]
primTeiler :: Integer -> [Integer] 
primTeiler n
			| n<2 = error "falsche Eingabe"
			| otherwise = [x | x <- (primzahlen n), (mod n x) == 0] --aus der Liste der Primzahlen werden diejenigen Elemente ausgewählt, die Teiler von n sind
--andere Möglichkeit


--Aufgabe 7
paintPicture :: ((Int, Int, Int) -> Char) -> Int -> [Char]
paintPicture f size = paint size (map f [(x,y,size) | x <- [1..size], y <- [1..size]]) size
                      where
                        paint 0  []    size = []
                        paint 0 (c:cs) size = '\n' : (paint size (c:cs) size)
                        paint n (c:cs) size = c: (paint (n-1) cs size)

diamond (x,y,size) = if abs(x-mitte) + abs(y-mitte) < mitte then '|'
						else ' '
						where 
							mitte = div size 2

circle (x,y,size) = if (x-mitte)^2 + (y-mitte)^2 < mitte*2 then '*'
						else ' '
						where
							mitte = div size 2
				

test1 = putStrLn (paintPicture diamond 41)
test2 = putStrLn (paintPicture circle 40)
		
--Aufgabe 8
--a
binom_naiv :: Integer -> Integer -> Integer
binom_naiv n k = if 0<=k && k<=n then div (fac n) ((fac k)*(fac (n-k)))
			else error "ungültige Werte"
fac :: Integer -> Integer
fac x
	| x<0 = error "nicht definiert"
	| x==0 = 1
	| otherwise = x*fac(x-1)

--b
binom_rek :: Integer -> Integer -> Integer
binom_rek n k
	| k>n =0
	| k==0 = 0
	| k==1 = n
	| otherwise = ((binom_rek (n-1) (k-1)) + (binom_rek (n-1) k))

--c
binom_rek2 :: Integer -> Integer -> Integer
binom_rek2 n k = round (binomHilf (fromIntegral n) (fromIntegral k) 1.0) --wegen den Bruechen Typ umwandeln
  where binomHilf :: Double -> Double -> Double -> Double
        binomHilf n 0 x = x --k als Zaehler mit Startwert 0, x akkumuliert die Werte
        binomHilf n k x = binomHilf (n-1) (k-1) (x*(n/k))   
	
--d
test_binom :: Integer -> Integer -> Bool
test_binom n k = ((binom_naiv n k) == (binom_rek n k)) && ((binom_naiv n k) == (binom_rek2 n k))




