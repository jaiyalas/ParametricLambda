{-# LANGUAGE PatternGuards #-}

module Lambda.Text.Substitution
    ( replaceable
    , replace
    , subs
    , alphaConv
    , renaming
    ) where
--
import Lambda.Text.Term
import Lambda.Text.InputSet

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

-- replaceable t u x |= replace T[U/x]
replace :: Term -> Term -> VName -> Term
replace (App p q) u x = App (replace p u x) (replace q u x)
replace (Var a)   u x | a == x = u
                      | a /= x = (Var a)
replace (Abs a t) u x | a == x, elem (Var a) (fv t)
                      = error $ "Dual binding of variable "++(show x) -- (Abs a t)
                      | a == x, notElem (Var a) (fv t)
                      = (Abs a t)
                      | a /= x = Abs a (replace t u x)

-- (\x.T) U is a redex |= subs t u x
subs :: Term -> Term -> VName -> Term
subs t u x | replaceable t u x       = replace t u x
           | not (replaceable t u x)
           , notElem (Var x) (fv t)  = t
           | not (replaceable t u x)
           , elem (Var x) (fv t)  = subs (renaming (fv u) t) u x

-- alpha conversion for some given name
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
renaming (t:ts) u | (Var a) <- t = renaming ts (alphaConv a u)
                  | otherwise   = error "non-variable show up from function \"renaming\""
