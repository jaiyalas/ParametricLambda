module Haha.Lambda.NamedVar.Reduction
    ( replaceable
    , replace
    , alphaConv
    , renaming
    ) where


import Haha.Lambda.NamedVar.Term
import Haha.Lambda.NamedVar.NameSugar
import Haha.Lambda.NamedVar.InputSet
import Haha.Lambda.NamedVar.Redex

--
-- T[U/x]
-- U is free for x in T
replaceable :: Term -> Term -> VName -> Bool
replaceable (Var _)   _ _ = True
replaceable (App p q) m x =
    replaceable p m x && replaceable q m x
replaceable (Abs y t) m x = x /= y
    && notElem (Var y) (fv m)
    && replaceable t m x

-- replacement T[U/x]
-- replaceable t u x |=
replace :: Term -> Term -> VName -> Term
replace (App p q) u x = App (replace p u x) (replace q u x)
replace (Var a)   u x | a == x = u
                      | a /= x = (Var a)
replace (Abs a t) u x | a == x = (Abs a t)
                      | a /= x = Abs a (replace t u x)

alphaConv :: VName -> Term -> Term
alphaConv z (App p q) = App (alphaConv z p) (alphaConv z q)
alphaConv z (Var a)   | z == a = Var (a++a)
alphaConv z (Var a)   | z /= a = Var a
alphaConv z (Abs a t) | z == a = Abs (a++a) (alphaConv z t)
alphaConv z (Abs a t) | z /= a = Abs a (alphaConv z t)

-- given a list of variables, TS, and a term U,
-- renaming every lambda binding listed on TS in U
renaming :: [Term] -> Term -> Term
renaming []           u = u
renaming (t:ts) u | (Var a)<- t = renaming ts (alphaConv a u)
                  | otherwise   = error "non-variable show up from function \"renaming\""
