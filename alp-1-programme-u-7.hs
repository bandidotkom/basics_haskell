{-- Vorlesungsbeispiel. Algebraische Datentypen --}
{-- Author: M. Esponda --}

module QRationals where

data Natural = Zero | S Natural
               deriving Show

add ::  Natural -> Natural -> Natural
add a Zero = a
add a (S b) = add (S a) b

foldn :: (a -> a) -> a -> Natural -> a
foldn h c Zero = c
foldn h c (S n) = h (foldn h c n)

nsucc :: Natural -> Natural
nsucc n = S n

npred :: Natural -> Natural
npred Zero = Zero
npred (S n) = n

cutOffSub :: Natural -> Natural -> Natural
cutOffSub m Zero = m
cutOffSub m (S n) = npred (cutOffSub m n)

plus :: Natural -> Natural -> Natural
plus = foldn nsucc

fact :: Natural -> Natural
fact Zero = S Zero
fact (S n) = foldn h (S Zero) n
             where
             h = mult n

pown :: Natural -> Natural -> Natural
pown m = foldn (mult m) (S Zero)

sub :: Natural -> Natural -> Natural
sub a Zero = Zero
sub (S a) (S b) = sub a b
sub Zero _ = error "not posible in Natural"

mult :: Natural -> Natural -> Natural
mult _ Zero = Zero
mult a (S b) = add a (mult a b)

npow :: Natural -> Natural -> Natural
npow b Zero  = S Zero
npow b (S e) = mult b (npow b e) 

factorial :: Natural -> Natural
factorial Zero = S Zero
factorial (S b) = mult (S b) (factorial b)

lt :: Natural -> Natural -> Bool
lt Zero (S _) = True
lt (S a) (S b) = lt a b
lt _ _ = False

gt :: Natural -> Natural -> Bool
gt (S _) Zero = True
gt (S a) (S b) = gt a b
gt _  _  = False

equal :: Natural -> Natural -> Bool
equal a b = (not (lt a b)) && (not (lt b a))

teiler :: Natural -> Natural -> Bool
teiler Zero _ = error "zero is not a valid divisor"
teiler a b | lt a b     = teiler a (cutOffSub b a)
           | equal a b  = True
           | otherwise  = False

{-- some naturals for testing --}
one = S Zero
two = S (S Zero)
three = S (S (S Zero))
four  = S (S (S (S Zero)))
five  = S (S (S (S (S Zero))))
six   = S (S (S (S (S (S Zero)))))
seven = S (S (S (S (S (S (S Zero))))))
eight = S (S (S (S (S (S (S (S Zero)))))))
nine  = S (S (S (S (S (S (S (S (S Zero))))))))
ten   = S (S (S (S (S (S (S (S (S (S Zero)))))))))

data ZInteger = Z Natural Natural
               deriving Show
               
zadd :: ZInteger -> ZInteger -> ZInteger
zadd (Z a b) (Z c d) = Z (add a c) (add b d)

zsub :: ZInteger -> ZInteger -> ZInteger
zsub (Z a b) (Z c d) = Z (add a d) (add b c)

{-- (a,b)*(c,d) equiv. (b-a)*(d-c) = bd - ad - cb + ac
                                   equiv.  (ad + cd, bd + ac)
--}

zmult :: ZInteger -> ZInteger -> ZInteger
zmult (Z a b) (Z c d) = Z (add (mult a d) (mult c b)) (add (mult a c) (mult b d)) 

zsimplify :: ZInteger -> ZInteger
zsimplify (Z Zero b) = Z Zero b
zsimplify (Z a Zero) = Z a Zero
zsimplify (Z (S a) (S b)) = zsimplify (Z a b)

{-- (a,b) kleiner gleich (c,d) <=>  (b-a) kleiner gleich (d-c)
                               <=>  (b+c) kleiner gleich (a+d)
--}

zlt :: ZInteger -> ZInteger -> Bool
zlt (Z a b) (Z c d) = lt (add b c) (add a d)

zequal :: ZInteger -> ZInteger -> Bool
zequal (Z a b) (Z c d) = equal (add b c) (add a d)

{-- some ZIntegers for testing --}

zone   =  Z Zero (S Zero)
ztwo   =  Z Zero (S (S Zero))
zthree =  Z Zero (S (S (S Zero)))
zfour  =  Z Zero (S (S (S (S Zero))))
zfive  =  Z Zero (S (S (S (S (S Zero)))))
zsix   =  Z Zero (S (S (S (S (S (S Zero))))))
zseven =  Z Zero (S (S (S (S (S (S (S Zero)))))))
zeight =  Z Zero (S (S (S (S (S (S (S (S Zero))))))))
znine  =  Z Zero (S (S (S (S (S (S (S (S (S Zero)))))))))
zten   =  Z Zero (S (S (S (S (S (S (S (S (S (S Zero))))))))))

{-- some negative ZIntegers for testing --}

mzone   =  Z (S Zero) Zero
mztwo   =  Z (S (S Zero)) Zero
mzthree =  Z (S (S (S Zero))) Zero
mzfour  =  Z (S (S (S (S Zero)))) Zero
mzfive  =  Z (S (S (S (S (S Zero))))) Zero
mzsix   =  Z (S (S (S (S (S (S Zero)))))) Zero
mzseven =  Z (S (S (S (S (S (S (S Zero))))))) Zero
mzeight =  Z (S (S (S (S (S (S (S (S Zero)))))))) Zero
mznine  =  Z (S (S (S (S (S (S (S (S (S Zero))))))))) Zero
mzten   =  Z (S (S (S (S (S (S (S (S (S (S Zero)))))))))) Zero

--Aufgabe 1
f :: (a->b)->(a->Bool)->[a]->[b]
f g p xs = (map g)((filter p) xs)

counterOfLength :: [[a]] -> Int -> Int
counterOfLength [] _ = 0
counterOfLength (x:xs) n
	|length x == n = 1 + counterOfLength xs n
	|otherwise = counterOfLength xs n

counterOfLength2 :: [[a]] -> Int -> Int
counterOfLength2 xs n = length(filter (==n) (map (length) xs))

--Aufgabe 2

matrix_map :: (a -> b) -> [[a]] -> [[b]]
matrix_map p xs = map (map p) xs

skalar_mult c xs = matrix_map (*c) xs

dim :: [[a]] -> (Int, Int)
dim xs = (length xs, length(head xs))
dim [] = (0,0)

--Aufgabe 3
{-# Language NPlusKPatterns #-}

sumQuad :: Int -> Int
sumQuad n = foldr (+) 0 (map(^2)[1..n])

myMin :: [Int] -> Int
myMin (x:xs) = foldr f x xs
	where
		f a b 
			|a>b= b
			|otherwise = a

--Aufgabe 4
durchschnitt :: [Double] -> Double -> Double -> Double
durchschnitt xs a b = (foldl (+) 0 list) / (fromIntegral(length list))
	where
	list = filter f xs
	f n = n>a && n<=b

--Aufgabe 5
data Natural = Zero | S Natural
		deriving Show

mult :: Natural -> Natural -> Natural
mult _ Zero = Zero
mult a (S b) = add a (mult a b)

add :: Natural -> Natural -> Natural
add a Zero = a
add a (S b) = add (S a) b

cutOffSub :: Natural -> Natural -> Natural
cutOffSub m Zero = m
cutOffSub m (S n) = npred (cutOffSub m n)

npred :: Natural -> Natural
npred Zero = Zero
npred (S a) = a

lt :: Natural -> Natural -> Bool
lt Zero (S _) = True
lt (S a) (S b) = lt a b
lt _ _ = False

pown :: Natural -> Natural -> Natural
pown _ Zero = S Zero
pown a (S b) = mult a (pown a b)

factorial :: Natural -> Natural
factorial Zero = S(Zero)
factorial (S a) = mult (S a) (factorial a)

teiler :: Natural -> Natural -> Bool
teiler Zero b = error "nicht definiert"
teiler a Zero = True
teiler a b | lt a b = teiler a (cutOffSub b a)
			|otherwise = False

--b
nat2Int :: Natural -> Integer
nat2Int Zero = 0
nat2Int (S a) = (+) (nat2Int a) 1

int2Nat :: Integer -> Natural
int2Nat 0 = Zero
int2Nat (n+1) = (S(int2Nat n))

--Aufgabe 6
unfold :: (a->Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold p f g x | p x = []
				|otherwise = f x : unfold p f g (g x)

dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin n = reverse (unfold (==0) ('mod' 2) ('div' 2) n)

















