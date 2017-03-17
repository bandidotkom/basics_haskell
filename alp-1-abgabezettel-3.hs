--ProInfo 2 Funktionale Programmierung SoSe 2013
--Abgabezettel 3, 29.07.2012
--Andras Pal Komaromy
--Tutor: David Bohn

--Aufgabe 1
myIterate1 :: Int -> [Bool]
myIterate1 y = take y (iterate (\x -> not x) True)

myIterate2 :: Int -> [Integer]
myIterate2 y = take y (iterate (\x -> (-2)*x) (2))

myIterate3 :: Int -> [Integer]
myIterate3 y = take y (iterate (\x -> x + ((round(sqrt(fromIntegral x))) + 1) * 2) 0)
{- Idee: Die Rechteckzahlen lassen sich auf folgende zwei Weisen darstellen: z.B. 6=2*3 oder 6=2+4; 12=3*4 oder 12=2+4+6 usw.
Zum Vorgänger in der Liste der Rechteckzahlen wird immer eine um 2 höhere Zahl addiert, die gleichzeitig das 2-fache des größeren Faktors in der Produktdasrtellung des Vorgängers ist.
Diesen größeren Faktor rechnet man wie folgt aus: (round(sqrt(x))+1, wobei man für x noch eine Typumwandlung wegen der Quadratwurzel berücksichtigen muss. -}

--Aufgabe 2
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p f g x
		| p x = []
		| otherwise = f x : unfold p f g (g x)

mapUnfold :: (Eq a) => (a -> a) -> [a] -> [a]
mapUnfold h xs =  unfold (== []) (\xs -> h (head xs)) (\xs -> tail xs) xs

iterateUnfold :: Int -> (a -> a) -> a -> [a]
iterateUnfold n k y = take n (unfold (\y -> False) (\y -> y) (\y -> k y) y)

int2binUnfold :: Integer -> [Integer]
int2binUnfold 0 = [0]
int2binUnfold n = reverse (unfold (==0) (\n -> mod n 2) (\n -> div n 2) n)

{- Aufgabe 3
T[/x.y(yxy)]
=> ST[/x.y]T[/x.yxy]  			Regel 4
=> S(KTy)T[/x.yxy]				Regel 3
=> S(KTy)(ST[/x.y]T[/x.x]T[/x.y]) 	Regel 4
=> S(Ky)(ST[/x.y]T[/x.x]T[/x.y])	Regel 1
=> S(Ky)(S(Ky)T[/x.x]T[/x.y])		Regel 3
=> S(Ky)(S(Ky)IT[/x.y])				Regel 2
=> S(Ky)(S(Ky)I(Ky))					Regel 3
-}

{- Aufgabe 4
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x:(++ xs ys)

/rxy.({NIL}yy(:({HEAD}x)(r({TAIL}x)y)))

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

/rx.({NIL}{NIL}(++(r({TAIL}x)({HEAD}x))))
-}

{- Aufgabe 5
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []								...tw.1
takeWhile p (x:xs) | p x = x : takeWhile p xs	...tw.2
					| otherwise = []			...tw.3

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []								...dw.1
dropWhile p (x:xs) | p x = dropWhile p xs		...dw.2
					| otherwise = (x:xs)		...dw.3

span :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])							...sp.1
span p (x:xs) | p x = (x:ys, zs)				...sp.2
				| otherwise = ([], x:xs)		...sp.3
					where (ys, zs) = span p xs	...sp.4

Behauptung: span p xs = (takeWhile p xs, dropWhuile p xs)
Beweis durch strukturelle Induktion über xs

IA: xs=[]
span p []=([],[]) mit sp.1 und
([],[]) = (takeWhile p [], dropWhile p []) mit tw.1, dw.1
=> IA richtig für []

IV: für xs=xs' gilt span p xs' = (takeWhile p xs', dropWhile p xs')

IS: xs=x:xs'
z.z.: span p (x:xs') = (takeWhile p (x:xs'), dropWhile p (x:xs'))

	Fall 1: (p x) == True
	linke Seite: span p (x:xs')=(x:ys, zs) mit sp.2
								= (x:takeWhile p xs', dropWhile p xs') mit sp.4 und IV
	rechte Seite: (takeWhile p (x:xs'), dropWhile p (x:xs')) = (x:takeWhile p xs', dropWhile p xs') mit tw.2 und dw.2
	
	Fall 2: (p x) == False
	linke Seite: span p (x:xs')= ([], x:xs') mit sp.3
	rechte Seite: (takeWhile p (x:xs'), dropWhile p (x:xs')) = ([], x:xs') mit tw.3 und dw.3

Somit gilt die Behauptung für alle xs.
-}

{- Aufgabe 6
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a)
		deriving Eq
mapTree :: (a -> b) -> Tree a -> Tree b									
mapTree f Nil = Nil														...mt.1
mapTree f (Leaf x) = Leaf (f x)											...mt.2
mapTree f (Node x lt rt) = (Node (f x) (mapTree f lt) (mapTree f rt))	...mt.3

tree2List :: Tree a -> [a]
tree2List Nil = []														...tl.1
tree2List (Leaf x) = [x]												...tl.2
tree2List (Node x lt rt) = tree2List lt ++ [x] ++ tree2List rt			...tl.3

map :: (a -> b) -> [a] -> [b]
map f [] = [] 															...map.1
map f (x:xs) = f x : map f xs											...map.2

Behauptung: map f (tree2List t) = tree2List (mapTree f t)

Beweis durch strukturelle Induktion für t

IA:
	1. t=Nil
		map f (tree2List Nil) = map f []  mit tl.1
							= []  mit map.1
		tree2List (mapTree f Nil) = tree2List Nil  mit mt.1
							= []  mit tl.1
	2. t=Leaf a
		map f (tree2List (Leaf x)) = map f [x]  mit tl.2
							= [f x] mit map.2
		tree2List (mapTree f (Leaf x)) = tree2List (Leaf (f x)) mit mt.2
							= [f x] mit tl.2

=> IA ist richtig für Nil und Leaf x

IV: für T= Node x lb rb gilt
	map f (tree2List lb) = tree2List (mapTree f lb)
	map f (tree2List rb) = tree2List (mapTree f rb)

IS: t=Node x lb rb

z.z.: map f (tree2List (Node x lb rb)) = tree2List (mapTree f (Node x lb rb))
	rechte Seite: tree2List (mapTree f (Node x lb rb)) = tree2List (Node (f x) (mapTree f lt) (mapTree f rt)) mit mt.3
								= tree2List (mapTree f lt) ++ [f x] ++ tree2List (mapTree f rt) mit tl.3
	
	linke Seite: map f (tree2List (Node x lb rb)) = map f (tree2List lb ++ [x] ++ tre2List rb)  mit tl.3
								= map f (tree2List lb) ++ map f [x] ++ map f (tree2List rb)  mit Hilfsresultat s. unten
								= tree2List (mapTree f lb) ++ [f x] ++ tree2List (mapTree f rb)  mit IV und map.2

Somit gilt die Behauptung für alle t=Node x lb rb

Hilfsresultat: map f (xs ++ ys) = map f xs ++ map f ys
Beweis durch Induktion über xs
IA: xs=[]
map f ([] ++ ys) = map f ys  mit (++).1
				= [] map f ys ++ map f ys mit map.1

IV: map f (xs' ++ ys) = map f xs' ++ map f ys  für alle xs= xs'
IS: xs=x:xs'
z.z.: map f (x:xs' ++ ys) = map f x:xs' ++ map f ys
	linke Seite: map f (x:xs' ++ ys) = f x : map f (xs'++ys) mit map.2
									= f x : (map f xs' ++ map f ys) mit IV
	rechte Seite: map f x:xs' ++ map f ys = f x : (map f xs') ++ map f ys mit map.2
									= [f x] ++ map f xs' ++ map f ys  nach Def. von :
									= [f x] ++ (map f xs' ++ map f ys)  wegen der Assoziativität von (++)
									= f x : (map f xs' ++ map f ys).
-}

{- Aufgabe 7
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []								...tw.1
takeWhile p (x:xs) | p x = x : takeWhile p xs	...tw.2
					| otherwise = []			...tw.3

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []								...dw.1
dropWhile p (x:xs) | p x = dropWhile p xs		...dw.2
					| otherwise = (x:xs)		...dw.3

Behauptung: (takeWhile p xs) ++ (dropWhile p xs) = xs

Beweis durch Induktion über xs
IA: xs=[]
(takeWhile p []) ++ (dropWhile p []) = [] ++ [] = [] mit tw.1 und dw.1
IA ist richtig für []

IV: für xs= xs' gilt (takeWhile p xs') ++ (dropWhile p xs') = xs'
IS: xs=x:xs'
z.z.: (takeWhile p (x:xs')) ++ (dropWhile p (x:xs')) = x:xs'
Fall 1: (p x) == True
	linke Seite: (takeWhile p x:xs') ++ (dropWhile p x:xs') = (x: takeWhile p xs') ++ (dropWhile p xs') mit tw.2 und dw.2
									= [x] ++ (takeWhile p xs' ++ dropWhile p xs') wegen der Assoziativität von (++)
									= x:xs' mit IV
Fall 2: (p x) == False
	linke Seite: (takeWhile p x:xs') ++ (dropWhile p x:xs') = [] ++ x:xs'   mit tw.3 und dw.3
									= x:xs'

Somit gilt die Behauptung für alle xs.
-}

{- Aufgabe 8
Seien h :: a -> a -> a
g :: a -> a -> a
z :: a

foldl _ v [] = v							...fl.1
foldl f v (x:xs) = foldl f (f v x) xs		...fl.2
Nehmen wir an, für alle x :: a gilt:
h x (g y z) = g z (h x y)					...a.1

Behauptung: h x (foldl g y zs) = foldl g (h x y) zs

Beweis durch Induktion über zs
IA: zs=[]
h x (foldl g y []) = h x y  mit fl.1, und
foldl g (h x y) [] = h x y mit fl.1
IA ist richtig für []

IV: h x (foldl g y zs') = foldl g (h x y) zs'  für alle zs=zs'

IS: zs=z:zs'
z.z.: h x (foldl g y z:zs') = foldl g (h x y) z:zs'
	linke Seite: h x (foldl g y z:zs') = h x (foldl (g y z) zs')  mit fl.2
									= foldl g (h x (g y z)) zs'  mit IV
									= foldl g (g z (h x y)) zs'  mit a.1
									= foldl g (g (h x y) z) zs' wegen der Kommutativität von g
	rechte Seite: foldl g (h x y) z:zs' = foldl g (g (h x y) z) zs'  mit fl.2
Somit gilt die Behauptung für alle zs.
-}