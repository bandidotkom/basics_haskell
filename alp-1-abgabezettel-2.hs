--ProInfo 2 Funktionale Programmierung SoSe 2013
--Abgabezettel 2, 22.07.2012
--Andras Pal Komaromy
--Tutor: David Bohn
--Aufgabe 2
--a
nmult n 0 = 0 --O(1)
nmult n m = nmult n (m-1) + n --O(m) linear, da T(m)=c*(m-1)
--insgesamt: O(m)

--b
bMult n 0 = 0 --O(1)
bMult n m | (m`mod`2)==0 = bMult (n+n)(m`div` 2)
		  | otherwise = bMult (n+n)(m `div` 2) + n


--Aufgabe 3
isort::[Integer]->[Integer]
isort []=[] --O(1)
isort (a:x) = ins a (isort x) --O(m*n)

ins::Integer->[Integer]->[Integer]
ins []=[a] --O(1)
ins a (b:y) | a<=b = a:(b:y) --O(n), da T(n)=c*(n-1), wobei n=length y
			| otherwise = b: (ins a y) --O(n) wie oben
--insgesamt: O(n^2), wenn n=m

--Aufgabe 4
--a
binSearch :: (Ord a) => a -> [a] -> Bool
binSearch b [] = False --O(1)
binSearch b [x] = b==x --O(1)
binSearch b xs
		| b<mitte = binSearch b links --O(logn), Begründung vgl. Folie 8 S. 30
		| b>mitte = binSearch b rechts --O(logn), wie oben
		| b==mitte = True --O(1)
			where
				half = (length xs) `div` 2 --O(n), da T(n)=n, wobei n=length xs
				links = take half xs --O(n), da T(n)=n/2
				rechts = drop half xs --O(n), da T(n)=n/2
				mitte = head rechts --O(1)
--insgesamt: O(n*logn)

--b
--1. Version
mergesortStart :: (Ord a) => [a] -> [a]
mergesortStart [ ] = mergesort 0 [ ]
mergesortStart xs = mergesort (length xs) xs
mergesort :: Integer -> [a]
mergesort _ [] = []
mergesort _ [x] = [x]
mergesort _ [x,y] = if x <= y then [x,y] else [y,x]
mergesort len xs = merge (mergesort h leftList) (mergesort (len-h) rightList)
where
h = len `div` 2
leftList = take h xs
rightList = drop h xs
merge (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
then x: (merge xs (y:ys))
else y: (merge (x:xs) ys)
--n Vergleiche, logn Schritte, insgesamt: O(nlogn)

--2. Version
mergeLists :: (Ord a) => [[a]] -> [[a]]
mergeLists [] = []
mergeLists [x] = [x]
mergeLists (x:y:xs) = (merge x y): mergeLists xs
mergeSort :: (Ord a) => [[a]] -> [[a]]
mergeSort [x] = [x]
mergeSort (x:y:xs) = mergeSort (mergeLists (x:y:xs))
startMergeSort :: (Ord a) => [a] -> [a]
startMergeSort xs = sortedList
where
[sortedList] = mergeSort (split xs)
split :: [a] -> [[a]]
split [] = []
split [x] = [[x]]
split (x:xs) = [x]: (split xs)
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x: (merge xs (y:ys))
| otherwise = y: (merge (x:xs) ys)


--Aufgabe 5
--a
(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) [] ys = []
(\\) (e:xs) ys
	| elem e ys = (\\) xs ys
	| otherwise = e:(\\) xs ys

--b
diffGen :: (Eq a) => [a] -> [a] -> [a]
diffGen xs ys = [k | k <- xs, not (elem (k ys))]

--c
diffFilt :: (Eq a) => [a] -> [a] -> [a]
diffFilt (x:xs) ys = filter (\x -> not (elem (ys))) xs

--d
minNatNotIn :: [Integer] -> Integer
minNatNotIn [] = error "falsche Eingabe"
minNatNotIn xs = head ([0..] \\ xs)
--e
-- minNatNotIn hat einen linearen Aufwand O(n), da T(n)=n-1, n=length xs in (\\) und c*O(n)=O(n) in minNatNotIn

--Aufgabe 6
--a
maxListFold :: (Ord a) => [a] -> a
maxListFold xs = foldl1 max xs -- Variante von foldl ohne Startwert

--oder nur mit fold:
maxListFold' :: (Ord a) => [a] -> a
maxListFold' (x:y:xs)
		| x<y = foldl max x xs
		| otherwise = foldl max y xs

--b
bin2decFold :: [Int] -> Int
bin2decFold xs = foldl f 0 xs --Startwert 0, Listenelemente werden von rechts mit einer immer höheren Potenz von 2 multipliziert
	where
	f :: Int -> Int -> Int
	f z x = 2*z + x


--Aufgabe 7
pack :: (Eq a) => [a] -> [(a,Int)]
pack [] = []
pack (x:xs) = packHilf (x,1) xs --Startwert: (erstes Element, Häufigkeit=1), rekursiv weiter mit Hilfsfunktion
		where
			packHilf (y,k) [] = [(y,k)] --wenn leere Liste erreicht, Übergabe des Tupels, k: Zähler für Häufigkeit
			packHilf (y,k) (l:ls)
				| y == l    = packHilf (y,k+1) ls --Element kommt wieder vor, Zähler wird erhöht, rekursiv weiter mit Rest
				| otherwise = (y,k) : packHilf (l,1) ls --Übergabe des Tupels als Listenelement, weiter mit dem nächsten Element

--Aufgabe 8
data Queue a = Queue [a]
			deriving Show
isEmpty (Queue []) = True
isEmpty _ = False

enqueue :: Queue a -> a -> Queue a
enqueue (Queue xs) y = Queue (xs ++ [y]) --neues Element wird am Ende hinzugefügt

dequeue :: Queue a -> Queue a --Element entfernen ist nur an der ersten Position möglich
dequeue (Queue []) = error "Queue ist leer"
dequeue (Queue (x:xs)) = Queue (xs) --Element wird immer am Anfang entfernt
		
makeQueue :: [a] -> Queue a --Liste in eine Warteschlange umwandeln
makeQueue [] = Queue []
makeQueue xs = Queue xs

--Aufgabe 9
isortFold :: [Integer] -> [Integer]
isortFold xs = foldr (ins) [] xs
		where
			ins:: (Ord a) => a -> [a] -> [a] --s. Vorlesungsfolien
			ins x []        = [x]
			ins x (y:ys)
				| x <= y    = x:(y:ys)
				| otherwise = y:(ins x ys)

{- Aufgabe 10
korrekt sind:
1 (b gebunden; y, z, w frei)
3 (a, z, y gebunden; x, b, c frei)
6 (z, y, x frei; in der Klammer: x gebunden; r, y frei)
7 (x, y, z, a gebunden; w, b frei -}

{-Aufgabe 11
L steht für Lamdba
a
=> (L.x.xx)(Labc.cbabc)(Lx.fxx)(fx)y
b
=> (Lx.x)(Lbc.b((Lfx.f(fx))bc))
=> (Lx.x)(Lbc.b((Lx.b(bx)c))
=> (Lx.x)(Lbc.b(bbc))
=> Lbc.b(bbc)

c
=>(Lsz.s(s(z)))(Lwxy.x(wxy))(Lab.b)
=> (Lz.((Lwxy.x(wxy))((Lwxy.x(wxy))(z))))(Lab.b)
=> ((Lwxy.x(wxy))((Lwxy.x(wxy))(Lab.b)))
=> ((Lwxy.x(wxy))(Lxy.x((Lab.b)xy)))
=> ((Lwxy.x(wxy))(Lxy.x((Lb.b)y)))
=> ((Lwxy.x(wxy))(Lxy.xy))
=> (Lxy.x((Lxy.xy)xy))
=> (Lxy.x((Ly.xy)y))
=> (Lxy.x(x(y)))
-}
