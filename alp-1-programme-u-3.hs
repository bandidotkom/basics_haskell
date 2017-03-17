--Aufgabe 1
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

--Aufgabe 2
makeSet :: [Int] -> [Int]
makeSet [] = []
makeSet (x:xs) = x:makeSet (removeElem x xs)

removeElem :: Int -> [Int] -> [Int]
removeElem _ [] = []
removeElem y (x:xs) | y==x = removeElem y xs
					| otherwise = x:removeElem y xs
			
--Aufgabe 3
myZip :: [Int]->[Int]->[(Int,Int)]
myZip [] ys        =  []
myZip xs []     =  []
myZip (x:xs)(y:ys) =  (x,y):myZip xs ys

--Aufgabe 4
type Menge = [Int]

elementOf :: Int -> Menge -> Bool
elementOf _ [] = False
elementOf y (x:xs) = y==x || elementOf y xs

teilmenge :: Menge -> Menge -> Bool
teilmenge _ [] = False
teilmenge [] _ = True
teilmenge (x:xs) ys = elementOf x ys && teilmenge xs ys

gleich :: Menge -> Menge -> Bool
gleich xs ys = teilmenge xs ys && teilmenge ys xs

vereinigung :: Menge -> Menge -> Menge
vereinigung [] ys = ys
vereinigung xs [] = xs
vereinigung xs ys = makeSet (xs++ys)

schnittmenge :: Menge -> Menge -> Menge
schnittmenge [] ys = []
schnittmenge xs [] = []
schnittmenge (x:xs) ys = if elementOf x ys then x: schnittmenge xs ys
							else schnittmenge xs ys

mengendifferenz :: Menge -> Menge -> Menge
mengendifferenz xs [] = xs
mengendifferenz [] ys = []
mengendifferenz xs (y:ys) = if elementOf y xs then removeElem y (mengendifferenz xs ys)
								else mengendifferenz xs ys

--Aufgabe 6
groupEquals :: [Int] -> [[Int]]
groupEquals [] = []
groupEquals (x:xs) = (genList x xs):(groupEquals (editList x xs))
	where
		genList z [] = [z]
		genList z (y:ys) | z == y = y:(genList z ys) --Liste generieren, solange Elemente gleich sind
						| otherwise = [z]
		editList a [] = []
		editList a (b:bs) | a==b = editList a bs --die gleichen Elemente am Anfang entfernen
						| otherwise = (b:bs)
