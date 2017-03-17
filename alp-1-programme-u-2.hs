--Rekursion
add :: Integer -> Integer -> Integer
add n 0 = n
add n m = add (succ n) (pred m)

mult :: Integer -> Integer -> Integer
mult n 0 = 0
mult n m = add n (mult n (pred m))

--Aufgabe 1
teil :: Int -> Int -> Bool
teil a b = if mod a b == 0
			then True
			else False

--Aufgabe 2
pyth :: Int -> Int -> Int -> Bool
pyth a b c  | a>0 && b>0 && c>0 = a^2==b^2+c^2 || b^2==a^2+c^2 || c^2==a^2+b^2
			| otherwise = error "Eingabe falsch"

--Aufgabe 3
monoton :: Int -> Int -> Int -> Bool
monoton a b c = a>b && b>c || a<b && b<c

--Aufgabe 4
mehrfach :: Int -> Int -> Int -> Bool
mehrfach a b c = mod c (a*b) == 0

--Aufgabe 5
klammer :: Char -> Bool
klammer a = "a"=="(" || "a"==")"

--Aufgabe 6
--Formel falsch

--Aufgabe 7
areaP :: Integer -> Double -> Double
areaP n s = (((fromIntegral n)*s*a)/2)
				where
					a = (s)/(2*tan(pi/ (fromIntegral n)))

--Aufgabe 8
type Fraction = (Integer, Integer)

frac_sum :: Fraction -> Fraction -> Fraction
frac_sum (a,b) (c,d) = kuerzen((a*d)+(c*b), b*d)

frac_mult :: Fraction -> Fraction -> Fraction
frac_mult (a,b) (c,d) = kuerzen (a*c, b*d)

frac_quotient :: Fraction -> Fraction -> Fraction
frac_quotient (a,b) (c,d) = kuerzen (a*d, b*c)

frac_power :: Fraction -> Integer -> Fraction
frac_power (a,b) n = kuerzen(a^n, b^n)

kuerzen :: Fraction -> Fraction
kuerzen (a,b) = (div a (ggT a b), div b (ggT a b))

ggT :: Integer -> Integer -> Integer
ggT a b | b==0 = a
		| a==0 = b
		| a>=b = ggT (mod a b) b
		| otherwise = ggT a (mod b a)

--Aufgabe 9
quersum :: Integer -> Integer
quersum 0 = 0
quersum a = (mod a 10) + quersum (div a 10)