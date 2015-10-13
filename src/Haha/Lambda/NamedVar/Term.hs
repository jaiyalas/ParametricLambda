module Haha.Lambda.NamedVar.Term
    (
    VName,
    Term(Abs,App,Var),
    pretty,
    fun,
    (<>)
    ) where

type VName = String

data Term = Abs VName Term
          | App Term Term
          | Var VName
          deriving (Eq,Ord)

instance Show Term where
    show (Var n)     = "[\"var\",\"" ++ n ++ "\"]"
    show (Abs n t)   = "[\"lam\",\"" ++ n ++ "\"," ++ show t ++ "]"
    show (App t1 t2) = "[\"app\"," ++ show t1 ++ "," ++ show t2 ++ "]"



pretty :: Term -> String
pretty (Var n)     = n
pretty (Abs n t)   = "(\\" ++ n ++ "." ++ pretty t ++ ")"
pretty (App t1 t2) = "(" ++ pretty t1 ++ " " ++ pretty t2 ++ ")"

(<>) :: Term -> Term -> Term
(<>) = App

fun :: VName -> Term -> Term
fun = Abs
