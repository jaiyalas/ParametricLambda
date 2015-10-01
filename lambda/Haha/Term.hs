module Haha.Term
    (
    v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,
    Term(Abs,App,Var,Atom),
    pretty,
    Nat(Succ,Zero),
    (<>),
    fun
    ) where

data Nat = Zero
         | Succ Nat
         deriving Eq

instance Show Nat where
  show = showNat 0

data Term = Abs Term
          | App Term Term
          | Var Nat
          | Atom

{- ["lam","x",["app",["var","x"],["var","x"]]] -}
instance Show Term where
  show = showTerm 0

showTerm :: Int -> Term -> String
showTerm _ Atom = "*"
showTerm _ (Var n) = "[\"var\",\""++(show n)++"\"]"
showTerm n (Abs t) = "[\"lam\",\"n"++(show n)++"\","++(showTerm (n+1) t)++"]"
showTerm n (App t1 t2) = "[\"app\","++(showTerm n t1)++","++(showTerm n t2)++"]"

pretty :: Term -> String
pretty = pretty' 0

pretty' :: Int -> Term -> String
pretty' _ Atom        = "*"
pretty' _ (Var n)     = show n
pretty' n (Abs t)     = "(\\n"++(show n)++"."++(pretty' (n+1) t)++")"
pretty' n (App t1 t2) = "("++(pretty' n t1)++" "++(pretty' n t2)++")"

{- TOOLS -}

n0 = Zero
n1 = Succ n0
n2 = Succ n1
n3 = Succ n2
n4 = Succ n3
n5 = Succ n4
n6 = Succ n5
n7 = Succ n6
n8 = Succ n7
n9 = Succ n8

v0 = Var n0
v1 = Var n1
v2 = Var n2
v3 = Var n3
v4 = Var n4
v5 = Var n5
v6 = Var n6
v7 = Var n7
v8 = Var n8
v9 = Var n9

showNat :: Int -> Nat -> String
showNat i Zero = "n"++(show i)
showNat i (Succ n) = showNat (i+1) n

(<>) :: Term -> Term -> Term
(<>) = App

fun :: Term -> Term
fun = Abs

{- TESTING -}

test0 = Var Zero
test1 = Abs test0
test2 = App (Abs test1) (test0)
