--- Lambda Calculus Parser and Interpreter
--- Original version: WS-97-98 Prof. Dr. Raul Rojas
--- modified WS-09-10 by: Prof. Dr. Margarita Esponda
--- modified WS-12-13 by: Prof. Dr. Margarita Esponda

module LambdaCalcul (Expr, parser, eval, show)
    where

-- Definition of an algebraic type for lambda expressions

data Expr = Var String | App Expr Expr | Lambda String Expr | Nil
           deriving Eq --Show)

type Parser = String -> Expr

instance Show Expr where show = show_expr 

------------------ show function of lambda-expressions ----------------

show_expr::Expr->String
show_expr (Var x)   =  x
show_expr (App x y) = "(" ++ (show_expr x) ++ (show_expr y) ++")"
show_expr (Lambda x y) = "(/" ++ x ++ "." ++ (show_expr y) ++")"

---------------------------------------------------------------------------
------------------------------ PARSER -------------------------------------
parser :: Parser
parser str = parse Nil str

parse :: Expr -> String -> Expr
parse Nil []                            = error "empty expression"
parse e   []                            = e    

parse Nil (a:r) | lowLetter a         = parse (Var [a]) r
parse e   (a:r) | lowLetter a         = parse (App e (Var [a])) r

parse Nil (a:r) | symbol a              = parse Nil ((macro a)++r)
parse e   (a:r) | symbol a              = parse e   ((macro a)++r)

parse Nil ('{':r)                       = parse Nil ((longmacro (fst a))++(tail(snd a)))
                                            where a = (break (=='}') r)
parse e   ('{':r)                       = parse e   ((longmacro (fst a))++(tail(snd a)))
                                            where a = (break (=='}') r)

parse Nil ('/':a:'.':r) | lowLetter a   =  Lambda [a] (parse Nil r)
parse e   ('/':a:'.':r) | lowLetter a   =  App e (Lambda [a] (parse Nil r))
parse Nil ('/':a:b:r) |   (lowLetter a) 
                       && (lowLetter b) = Lambda [a] (parse Nil ('/':b:r))
parse e   ('/':a:b:r) |   (lowLetter a)
                       && (lowLetter b) = App e (Lambda [a] (parse Nil ('/':b:r)))
parse Nil ('(':r)                       = parse (parse Nil a) b
                                              where (a,b) = extract [] r 0
parse e   ('(':r)                       = parse (App e (parse Nil a)) b
                                              where (a,b) = extract [] r 0
parse e _ = error ("parsing error "++(show_expr e))

---------------------Auxiliary Functions ------------------------------

lowLetter x = elem x ['a'..'z']
digit x = elem x ['0'..'9']

symbol x = elem x ['A'..'Z'] || digit x ||
           elem x ['+','*','!','&','|','%','=','>','<','-','?',':']

extract :: String -> String -> Int -> (String, String)
extract a   []     _  = error "unbalanced parentheses"
extract a (')':b)  0  = (a,b)
extract a (')':b)  n  = extract  (a++")")  b (n-1)
extract a ('(':b)  n  = extract  (a++"(")  b (n+1)
extract a (b:c)    n  = extract  (a++[b])  c  n

-- returns the list of the free variables of an expression ----------------------

freeList::Expr->[String]->[String]
freeList (Var x)  bound | elem x bound = []
                        | otherwise    = [x]
freeList (Lambda x y) bound            = freeList y (x:bound)
freeList (App x y)    bound            = (freeList x bound)++(freeList y bound)

-- returns the list of the bounded variables of an expression --------------------

boundList ::Expr -> [String]
boundList (Var x)      = []                          
boundList (Lambda x y) = x:(boundList y)
boundList (App x y)    = (boundList x) ++ (boundList y)                              

-- finds a name which is not in the list of prohibited names -----------------

find_new_name::[String]->String
find_new_name prohibited = head [[u]|u<-['a'..'z'], not( elem [u] prohibited ) ]

-- renames variable y with the new name nn in the expression -----------------

rename::String->String->Expr->Expr
rename y nn (Var x)
                   | x==y      = Var nn
                   | otherwise = Var x
rename y nn (Lambda x z) 
                   | x==y      = (Lambda nn (rename y nn z))
                   | otherwise = (Lambda x (rename y nn z))
rename y nn (App x z) = (App (rename y nn x) (rename y nn z))

-- substitute variable (x) with expression (exp) in the expression of the
-- third argument, taking into account the free variables in exp

subst::String->Expr->Expr->Expr
subst x exp (Var y) 
            | x==y      =  exp
            | otherwise = Var y

subst x exp (Lambda y z)
           | x==y                       =  Lambda y z
           | (elem y (freeList exp [])) = subst x exp (rename y newname (Lambda y z))
           | otherwise                  = (Lambda y (subst x exp z))
                where newname = find_new_name ((freeList exp [])++(freeList z [])++(boundList z))

subst x exp (App y z) = App (subst x exp y) (subst x exp z)

--------------------------------------------------------------------------
-------- Evaluate a lambda expression using a call-by-name strategie -----
-------- eval-Function (Version 1) ---------------------------------------

eval0 :: Expr->Expr
eval0 (Var x)                = Var x  
         
eval0 (Lambda x exp)         = Lambda x exp

eval0 (App  (Var x)  exp)    = App (Var x) (eval0 exp)
eval0 (App (Lambda a e1) e2) = eval0 (subst a e2 e1)
eval0 (App (App e1 e2) e3)
           | (evale12 == (App e1 e2))   = App (App e1 e2) (eval0 e3)  
           | otherwise                  = eval0 (App evale12 e3)
                                          where evale12 = eval0 (App e1 e2)

--------- eval-Function (Version 2) --------------------------------------

eval :: Expr -> Expr
eval = evalB False

evalB :: Bool -> Expr -> Expr
evalB  _   (Var x)            = Var x                          
evalB  False  (Lambda x exp)  = Lambda x (evalB False exp)
evalB  True   (Lambda x exp)  = Lambda x exp
evalB  b  (App  e1 e2)
        | is_lambda eval_e1   = evalB b (subst (getarg eval_e1) e2 (getbody eval_e1))
        | otherwise           = (App eval_e1 (evalB False e2))
                                where
                                     eval_e1 = evalB True e1
                                     getarg  (Lambda x body)  = x
                                     getbody (Lambda x body)  = body

------------ auxiliary function ------------------------------------------

is_lambda::Expr->Bool
is_lambda (Lambda a b) = True
is_lambda  x           = False

---------------------------------------------------------------------------
-- lci is the complete lambda-calcul interpreter --------------------------
-- parse, evaluate and show a lambda expression ---------------------------

lci :: String -> Expr
lci = eval.parser

lci0 :: String -> Expr
lci0 = eval0.parser

---------------------------------------------------------------------------
-- natural numbers 
macro '0' = "(/sz.z)"
macro '1' = "(/sz.s(z))"
macro '2' = "(/sz.s(s(z)))"
macro '3' = "(/sz.s(s(s(z))))"
macro '4' = "(/sz.s(s(s(s(z)))))"
macro '5' = "(/sz.s(s(s(s(s(z))))))"
macro '6' = "(/sz.s(s(s(s(s(s(z)))))))"
macro '7' = "(/sz.s(s(s(s(s(s(s(z))))))))"
macro '8' = "(/sz.s(s(s(s(s(s(s(s(z)))))))))"
macro '9' = "(/sz.s(s(s(s(s(s(s(s(s(z))))))))))"

-- succesor function
macro 'S' = "(/wxy.x(wxy))"

-- arithmetic and logic functions
macro '+' = "(/xy.xSy)"         -- addition
macro '*' = "(/xyz.x(yz))"      -- multiplication
macro 'T' = "(/ab.a)"           -- true
macro 'F' = "(/ab.b)"           -- false
macro '&' = "(/xy.xyF)"         -- and
macro '|' = "(/xy.xTy)"         -- or
macro 'N' = "(/x.xFT)"          -- not
macro 'Z' = "(/x.xFNF)"         -- compare to 0
macro 'G' = "(/xy.Z(xPy))"      -- greater as
macro '=' = "(/xy.&(Gxy)(Gyx))" -- equal

-- predecesor function
macro 'P' = "(/n.nH(/z.z00)F)"   -- predecessor
macro 'H' = "(/pz.z(S(pT))(pT))" -- (n,n-1) to (n,n+1)

-- recursion operator for call-by-name
macro 'Y' = "(/r.(/x.r(xx))(/x.r(xx)))"

-- recursion operator for call-by-value
macro 'R' = "(/r.(/x.r(/y.(xxy)))(/x.r(/y.(xxy))))"

-- factorial
macro '!' = "(/rn.Zn1(*n(r(Pn))))"     --factorial

-- functions for lists
macro 'L' = "(/z.zTFF)"                --nil
macro ':' = "(/abz.zFab)"              --cons

-- macros with long names
longmacro "SUM" =  "(/rn.Zn0(+n(r(Pn))))"                 -- recursive sum
longmacro "FIB" =  "(/rn.=n00(=n11(+(r(Pn))(r(P(Pn))))))" -- fibonacci

-- more functions for lists
longmacro "NIL" = "(/x.x(/abc.a))"                     -- test nil
longmacro "TAIL" = "(/x.x(/abc.c))"                    -- tail
longmacro "HEAD" = "(/x.x(/abc.b))"                    -- head
longmacro "LEN"  = "(/rl.{NIL}l0(S(r({TAIL}l))))"      -- length

-- some lists for testing
longmacro "L1" = "(/z.zF1L)"
longmacro "L2" = "(/z.zF2{L1})"
longmacro "L3" = "(/z.zF3{L2})"

-- some integer numbers
longmacro "0" = "(/z.z00)"                          -- (0,0)
longmacro "1" = "(/z.z01)"                          -- (0,1)
longmacro "2" = "(/z.z02)"                          -- (0,2)
longmacro "-1" = "(/z.z10)"                         -- (1,0)
longmacro "-2" = "(/z.z20)"                         -- (2,0)

-- some functions for integer numbers
longmacro "ZADD" = "(/ab.(/z.z(+(aT)(bT))(+(aF)(bF))))"  -- add (a,b) (c,d) = (a+c,b+d)
longmacro "Zadd" = "(/ab.(/z.z((aT)S(bT))((aF)S(bF))))"  -- a second definition for the add function
longmacro "ZSUB" = "(/ab.{ZADD}a(/z.z(bF)(bT)))"         -- sub (a,b) (c,d) = add (a,b)(d,c)

list = "(:1(:2(:3L)))"

-- Anonyme functions in Haskell

f = \x -> x+1    ---  f x = exp
g = \x y -> x+y ---   g x y =  exp  
       
fact =  \n -> foldl  (\x y -> x*y) 1 [1..n] 
zf  = zipWith  (\x y -> x*x + y*y) 
h  =  map  (\xs -> zip xs [1..])

-------

fix rec = rec (fix rec) -- (Y-Function)

true = \x y -> x
false = \x y -> y

rec = \f n -> if n==0 then 1 else ((*) n (f (n-1)))
fibs = \r n -> if n<=1 then n else ((+) (r (n-1)) (r (n-2)))

test_fact = (fix rec) 9
test_fibs = (fix fibs) 9

-- Natural number <-> Lambda
num2Lam :: Integer -> String
num2Lam n = "(/xy."++num2Lam' n++")"
                where
                   num2Lam' 0 = "y"
                   num2Lam' n = "x("++num2Lam' (n-1)++")"

lambda2Num :: String -> Integer
lambda2Num  = toNum.dropWhile (/='.')
              where
                 toNum (')':ls) = 0
                 toNum ('(':ls) = 1 + toNum ls
                 toNum ls       = toNum (tail ls)
