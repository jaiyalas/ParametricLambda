{-# LANGUAGE BangPatterns #-}

import Haha.Term

subs :: Term -> Term -> Term
subs Atom        t = Atom
subs (Var Zero)  t = t
subs (Var n)     t = Var n
subs (Abs fb)    t = Abs fb
subs (App t1 t2) t = App (subs t1 t) (subs t2 t)
