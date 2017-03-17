
module SKII (Expr, Show, transform)
         where 

-- extended algebraic type for the transform function

data Expr = App Expr Expr | S | K | I | Var String | Lambda String Expr | Nil
              deriving Eq

instance Show Expr -- for Lambda and SKI expressions
        where show = show_expr 

show_expr S  = "S"
show_expr K  = "K"
show_expr I  = "I"

show_expr (Var x)      = x
show_expr (App x y)    = "("++(show_expr x)++(show_expr y)++")"
show_expr (Lambda x y) = "(/" ++ x ++ "." ++ (show_expr y) ++")"

-- returns the list of free variables of an expression

freie::Expr->[String]->[String]
freie (Var x)   bound | elem x bound = []
                      | otherwise       = [x]
freie (Lambda x y) bound = freie y (x:bound)
freie (App x y) bound = (freie x bound)++(freie y bound)
freie S bound = []
freie K bound = []
freie I bound = []

-------------------------------------------------------------------------------          

transform :: Expr -> Expr
transform S = S
transform K = K
transform I = I
transform (Var x)      = Var x
transform (Lambda x y) = (eliminate x y)
transform (App x y)    = App (transform x) (transform y)

--------------------------------------------------------------------------------

eliminate :: String -> Expr -> Expr
eliminate x S = App K S
eliminate x K = App K K
eliminate x I = App K I

eliminate x y       | not (elem x (freie y [])) = (App K (transform y))
eliminate x (Var y) | x==y      = I
                    | otherwise = (App K (Var y))

eliminate x (Lambda y z)        = eliminate x (eliminate y z)
eliminate x (App y z)           = (App (App S (eliminate x y)) (eliminate x z))

-------------------------------------------------------------------------------

zzero = transform (Lambda "s" (Lambda "z" (Var "z")))
succesor = transform (Lambda "w" (Lambda "x" (Lambda "y"
                         (App (Var "x") (App (App (Var "w") (Var "x")) (Var "y"))) )))

one = transform (Lambda "x" (Lambda "z" (App (Var "x") (Var "z"))))
two = transform (Lambda "x" (Lambda "z" (App (Var "x") (App (Var "x") (Var "z")))))
three = transform (App succesor two)
four = transform (App succesor three)
five = transform (App succesor four)
six = transform (App succesor five)
seven = transform (App succesor six)
eight = transform (App succesor seven)

exp1 = transform (Lambda "x" (App (Var "y") (App (Var "x") (Var "y"))) ) -- (/x.y(xy))

exp2 = transform (Lambda "z" (Lambda "x" (App (App (Var "y") (App (Var "x") (Var "y"))) (Var "z")) ))
            --(/zx.y(xy)z)

testnum = transform (Lambda "x" (App (App (Var "x") (Var "s")) (Var "z")))

mult = transform (Lambda "x" (Lambda "y" (Lambda "z" (App (Var "x") (App (Var "y") (Var "z"))) )))

true  = transform (Lambda "a" (Lambda "b" (Var "a")))
false = transform (Lambda "a" (Lambda "b" (Var "b")))
and2  = transform (Lambda "x" (Lambda "y" (App (App (Var "x") (Var "y")) false) ))
or2   = transform (Lambda "x" (Lambda "y" (App (App (Var "x")    true) (Var "y")) ))
not2  = transform (Lambda "x" (App (App (Var "x") false) true) )


test_zero = transform (Lambda "x" (App(App(App (Var "x") false) not2) false))

par_00 = transform (Lambda "z" (App(App (Var "z") zzero) zzero))
phi    = transform (Lambda "p" (Lambda "z" 
                   (App ( App (Var "z") (App succesor (App (Var "p") true))) (App (Var "p") true)  )))
prede = transform (Lambda "n" (App (App (App (Var "n") phi) par_00) false))

greater_eq = transform (Lambda "x" (Lambda "y" (App test_zero (App (App (Var "x") prede) (Var "y"))) ))

rec = transform (Lambda "y" (App (Lambda "x" (App (Var "y") (App (Var "x") (Var "x"))))
                    (Lambda "x" (App (Var "y") (App (Var "x") (Var "x"))))
                  )
                )

summe = transform (Lambda "r" (Lambda "n" (App (App (App test_zero (Var "n")) zzero)
                  (App (App (Var "n") succesor) (App (Var "r") (App prede (Var "n")))))))
      
fakul = transform (Lambda "r" (Lambda "n" (App (App (App test_zero (Var "n")) one)
                  (App (App mult (Var "n")) (App (Var "r") (App prede (Var "n")))))))

xc = transform (Lambda "f" (App (App (App (Var "f") K) S) K))


