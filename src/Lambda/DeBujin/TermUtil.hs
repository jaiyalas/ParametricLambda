module Haha.Lambda.TermUtil where

import Haha.Lambda.Term
import Haha.Lambda.TermSugar

fv :: Term -> [Term]
fv = fv' Zero

fv' :: Nat -> Term -> [Term]
fv' i (Var n)
    | i <= n = [Var n]
    | i >  n = []
fv' i (Abs t)     = fv' (Succ i) t
fv' i (App t1 t2) = fv' i t1 ++ fv' i t2
fv' i Atom        = []

freeIn :: Term -> Term -> Term -> Bool
freeIn m (Var x) (Var a) = True
freeIn m (Var x) (App p q) = freeIn m (Var x) p && freeIn m (Var x) q
freeIn m (Var x) (Lam n) = undefined
freeIn m (Var x) Atom = True
