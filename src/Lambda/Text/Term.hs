{-# LANGUAGE PatternGuards #-}

module Lambda.Text.Term
    ( VName
    , Term (..)
    , pretty
    , fun
    , (<>)
    , (~>)
    , fv
    , binder
    ) where

import qualified Data.Set as Set

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

(~>) :: VName -> Term -> Term
(~>) = Abs

infixr 1 ~>
infixl 2 <>

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
