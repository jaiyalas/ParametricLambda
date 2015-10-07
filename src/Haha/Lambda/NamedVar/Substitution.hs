 {-# LANGUAGE UnicodeSyntax #-}

module Haha.Lambda.NamedVar.Substitution
    (
    fv,
    replaceable,
    subs,
    InputSet,
    inInputSet,
    inΓ,
    inγ,
    inΛI
    ) where

import Haha.Lambda.NamedVar.Term

fv :: Term -> [Term]
fv (Var n)     = [Var n]
fv (Abs n t)   = filter (/= (Var n)) $ fv t
fv (App t1 t2) = fv t1 ++ fv t2

replaceable :: Term -> Term -> Term -> Bool
replaceable (Var a)   m (Var x) = True
replaceable (App p q) m (Var x) =
    replaceable p m (Var x) && replaceable q m (Var x)
replaceable (Abs y t) m (Var x) = x /= y &&
    notElem (Var y) (fv m) && replaceable t m (Var x)

subs :: Term -> Term -> VName -> Term
subs (App t u) tM x = App (subs t tM x) (subs u tM x)
subs (Abs y t) tM x = if y == x then Abs y t
                                else Abs y (subs t tM x)
subs (Var y)   tM x = if y == x then tM else Var y

red :: InputSet -> Term -> Term
red sΔ (App (Abs x tM) tN) = if inInputSet sΔ $ tN then subs tM tN x else tM
red sΔ others = others

-- Γ γ Ξ ξ Δ δ Λ λ

data InputSet = ISΛ | ISΓ | ISγ | ISΛI

inInputSet :: InputSet -> Term -> Bool
inInputSet ISΛ  = const True
inInputSet ISΓ  = inΓ
inInputSet ISγ  = inγ
inInputSet ISΛI = inΛI

inΓ :: Term -> Bool
inΓ (Var x)   = True
inΓ (Abs x t) = True
inΓ _         = False

inγ :: Term -> Bool
inγ (Var x)   = True
inγ (Abs x t) = elem (Var x) (fv t)
inγ _         = False

inΛI :: Term -> Bool
inΛI (Var x)   = True
inΛI (Abs x t) = elem (Var x) (fv t) && inΛI t
inΛI (App t u) = inΛI t && inΛI u
