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

data QRational = Q ZInteger ZInteger
                  deriving Show

{-- (z1,n1) + (z2,n2) equiv. z1/n1 + z2/n2 = (z1*n2 + z2*n1)/(n1*n2)
                      equiv. (z1*n2 + z2*n1, n1*n2)
--}

radd :: QRational -> QRational -> QRational
radd (Q z1 n1) (Q z2 n2) = Q (zadd (zmult z1 n2) (zmult z2 n1)) (zmult n1 n2)

requal :: QRational -> QRational -> Bool
requal (Q z1 n1) (Q z2 n2) = zequal (zmult z1 n2) (zmult n1 z2)

{-- some rational numbers for testing --}

rzero = Q (Z Zero Zero) (Z Zero (S Zero))
rone  = Q (Z Zero (S Zero)) (Z Zero (S Zero))
rtwo  = Q (Z Zero (S (S Zero))) (Z Zero (S Zero))
rthree = Q (Z Zero (S (S (S Zero)))) (Z Zero (S Zero))




