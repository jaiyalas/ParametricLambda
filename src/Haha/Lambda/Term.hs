module Haha.Lambda.Term
    (
    Term(Abs,App,Var,Atom),
    pretty,
    Nat(Succ,Zero),
    ) where

data Nat = Zero
         | Succ Nat
         deriving (Eq, Ord)

instance Show Nat where
  show = showNat 0

data Term = Abs Term
          | App Term Term
          | Var Nat
          | Atom

instance Show Term where
  show = showTerm 0

showTerm :: Int -> Term -> String
showTerm _ Atom = "*"
showTerm _ (Var n) = "[\"var\",\"" ++ show n ++ "\"]"
showTerm n (Abs t) = "[\"lam\",\"n" ++ show n ++ "\"," ++ showTerm (n+1) t ++ "]"
showTerm n (App t1 t2) = "[\"app\"," ++ showTerm n t1 ++ "," ++ showTerm n t2 ++ "]"

pretty :: Term -> String
pretty = pretty' 0

pretty' :: Int -> Term -> String
pretty' _ Atom        = "*"
pretty' _ (Var n)     = show n
pretty' n (Abs t)     = "(\\n" ++ show n ++ "." ++ pretty' (n+1) t ++ ")"
pretty' n (App t1 t2) = "(" ++ pretty' n t1 ++ " " ++ pretty' n t2 ++ ")"

showNat :: Int -> Nat -> String
showNat i Zero = "n" ++ show i
showNat i (Succ n) = showNat (i+1) n
