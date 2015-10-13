module Haha.Lambda.NamedVar.BigLambdaEagerReduction
    ( fv
    , fv_set
    , binder
    , binder_set
    , replaceable
    , replace
    , alphaConv
    , betaRed
    ) where

import qualified Data.Set as Set

import Haha.Lambda.NamedVar.Term

fv :: Term -> [Term]
fv = (Set.toAscList).fv_set

fv_set :: Term -> Set.Set Term
fv_set (Var n)     = Set.singleton (Var n)
fv_set (Abs n t)   = Set.filter (/= (Var n)) $ fv_set t
fv_set (App t1 t2) = Set.union (fv_set t1) (fv_set t2)

binder :: VName -> Term -> [Term]
binder x t = Set.toAscList $ binder_set x t

binder_set :: VName -> Term -> Set.Set Term
binder_set x (Var a) = Set.empty
binder_set x (Abs a t) | x == a = Set.empty
binder_set x (Abs a t) | x /= a = Set.insert (Var a) $ binder_set x t
binder_set x (App p q) = Set.union (binder_set x p) (binder_set x q)

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
renaming ((Var a):ts) u = renaming ts (alphaConv a u)

betaRed :: Term -> Term
betaRed (Var y)   = (Var y)
betaRed (Abs y t) = Abs y (betaRed t)
betaRed (App p q) | Abs a t <- betaRed p
                  , red     <- betaRed q
                  , replaceable t red a
                  = betaRed $ replace t red a
betaRed (App p q) | Abs a t <- betaRed p
                  , red     <- betaRed q
                  , not $ replaceable t red a
                  = betaRed $ App (renaming (fv red) p) red
betaRed (App p q) | otherwise = App p (betaRed q)
