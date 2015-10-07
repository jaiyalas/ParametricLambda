{-# LANGUAGE BangPatterns #-}

import Haha.Term
import Haha.TermSugar
import Haha.TermUtil

subs :: Term -> Term -> Term
subs Atom        t = Atom
subs (Var Zero)  t = t
subs (Var n)     t = Var n
subs (Abs fb)    t = Abs fb
subs (App t1 t2) t = App (subs t1 t) (subs t2 t)

myI = fun v0
myK = fun (fun v0)
myW = fun $ fun $ (v0 <> v1) <> v1
myC = fun $ fun $ fun $ (v0 <> v2) <> v1
myB = fun $ fun $ fun $ v0 <> (v1 <> v2)
myS = fun $ fun $ fun $ (v0 <> v2) <> (v1 <> v2)
