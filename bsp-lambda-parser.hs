--- Lambda Calculus Parser and Interpreter
--- Original version: WS-97-98 Prof. Dr. Raul Rojas
--- modified WS-09-10 by: Prof. Dr. Margarita Esponda

module LambdaCalcul (Expr, parser, eval, show, lci)
    where

-- Definition of an algebraic type for lambda expressions

data Expr = Var String | App Expr Expr | Lambda String Expr | Nil
            deriving (Eq)

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
parse e _ = error ("parsing error"++(show_expr e))

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

-- returns the list of free variables of an expression ----------------------

freeList::Expr->[String]->[String]
freeList (Var x)   bound 
                      | elem x bound    = []
                      | otherwise       = [x]
freeList (Lambda x y) bound = freeList y (x:bound)
freeList (App x y) bound = (freeList x bound)++(freeList y bound)

-- returns the list of bounded variables in an expression --------------------

boundList::Expr->[String]->[String]
boundList (Var x)   bound = []                          
boundList (Lambda x y) bound = x:(boundList y (x:bound))
boundList (App x y) bound = (boundList x bound)++(boundList y bound)                              

-- finds a name which is not in the list of prohibited names -----------------

find_new_name::[String]->String
find_new_name prohibited = head [[u]|u<-['a'..'z'], not( elem [u] prohibited ) ]

-- renames variable y with the new name n in the expression -----------------

rename::String->String->Expr->Expr
rename y n (Var x)
                   | x==y      = Var n
                   | otherwise = Var x
rename y n (Lambda x z) 
                   | x==y      = (Lambda n (rename y n z))
                   | otherwise = (Lambda x (rename y n z))
rename y n (App x z) = (App (rename y n x) (rename y n z))

-- substitute variable (x) with expression (exp) in the expression of the
-- third argument, taking into account the free variables in exp

subst::String->Expr->Expr->Expr
subst x exp (Var y) 
            | x==y      =  exp
            | otherwise = Var y

subst x exp (Lambda y z)
           | x==y                       =  Lambda y z
           | (elem y (freeList exp [])) = subst x exp (rename y new (Lambda y z))
           | otherwise                  = (Lambda y (subst x exp z))
                where new = find_new_name ((freeList exp [])++(freeList z [])++(boundList z []))

subst x exp (App y z) = App (subst x exp y) (subst x exp z)

-----------------------------------------------------------------------
-- Evaluate a lambda expression

eval::Expr->Expr

eval (Var x)               = Var x 
                 
eval (Lambda x exp)         = (Lambda x (eval exp))

eval (App  (Var x)  exp)   = (App (Var x) (eval exp))

eval (App (Lambda a e1) e2) = eval (subst a e2 e1)

eval (App (App e1 e2) e3)
           | (evale12 == (App e1 e2))   = (App (App e1 e2)  e3)  
           | otherwise                  = eval (App evale12 e3)
                 where evale12 = eval (App e1 e2)

---------------------------------------------------------------------------
-- lci is the complete lambda-calcul interpreter --------------------------
-- parse, evaluate and show a lambda expression ---------------------------

lci :: String -> Expr
lci = eval.parser

---------------------------------------------------------------------------
-- natural numbers
macro '0' = "(/s.(/z.z))"
macro '1' = "(S0)"
macro '2' = "(S1)"
macro '3' = "(S2)"
macro '4' = "(S3)"
macro '5' = "(S4)"
macro '6' = "(S5)"
macro '7' = "(S6)"
macro '8' = "(S7)"
macro '9' = "(S8)"

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

-- recursion operator
macro 'Y' = "(/y.(/x.y(xx))(/x.y(xx)))"

-- macros with long names
longmacro "FIB" =  "(/rn.=n00(=n11(+(r(Pn))(r(P(Pn))))))" -- fibonacci
longmacro "SUM" =  "(/rn.Zn0(+n(r(Pn))))"                 -- recursive sum

num :: Int -> String
num 0 = "/s.(/z.z))"
num n = "/s.(/z."++ (rep "s(" n) ++ "z)" ++ (rep ")" n)

rep  _  0 = ""
rep str n = str ++ (rep str (n-1))

