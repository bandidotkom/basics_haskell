data ZInteger = Z Natural Natural
		deriving Show


--Aufgabe 1
zint2Int :: ZInteger -> Integer
zint2Int (Z a b) = (nat2Int a) - (nat2Int b)

int2Zint :: Integer -> ZInteger
int2Zint n | n<=0 = Z Zero (int2Nat (-n))
			| otherwise = Z (int2Nat n) Zero

zpow :: ZInteger -> Natural -> ZInteger
zpow (Z a b) Zero = (Z (S Zero) Zero)
zpow (Z a b) S n = zmult (Z a b) (zpow (Z a b) n)

zabs :: ZInteger -> ZInteger
zabs (Z a b) | lt a b = zsimplify (Z b a)
			| otherwise = zsimplify (Z b a)

zsimplify :: ZInteger -> ZInteger
zsimplify (Z Zero b) = Z Zero b
zsimplify (Z a Zero) = Z a Zero
zsimplify (Z S(a) S(b)) = zsimplify (Z a b)

--Aufgabe 2
insertLeaf :: SBTree -> SBTree
insertLeaf L = (N L L)
insertLeaf (N lb rb)
		| nodes lb >= nodes rb = N lb (insertLeaf rb)
		| otherwise = N (insertLeaf lb) rb
		
depth:: SBTree -> Integer
depth L = 0
depth (N lb rb) = 1 + (max ((depth lb) (depth rb)))

insertLeafs :: SBTree -> Integer -> SBTree
insertLeafs tr 0 = tr
insertLeafs tr n = insertLeafs (insertLeaf tr) (n-1)

deleteLeaf :: SBTree -> SBTree
deleteLeaf (N L L) = L
deleteLeaf (N lb rb)
	| nodes lb < nodes rb = N lb (deleteLeaf rb)
	| otherwise = N (deleteLeaf lb) rb


