module Haha.Lambda.Sugar
    (
    v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,
    (<>),
    fun
    ) where

import Haha.Lambda.Term

(<>) :: Term -> Term -> Term
(<>) = App

fun :: Term -> Term
fun = Abs

n0, n1, n2, n3, n4 :: Nat
n5, n6, n7, n8, n9 :: Nat
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

v0, v1, v2, v3, v4 :: Term
v5, v6, v7, v8, v9 :: Term
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
