{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Set as Set

type VName = String
data Nat   = Z | S Nat

data OldTerm = OAbs VName OldTerm
             | OApp OldTerm OldTerm
             | OVar VName
             deriving (Eq,Ord)

class LTerm vt tt | vt -> tt where
  v  :: vt -> tt vt
  (~>) :: vt -> (tt vt) -> (tt vt)
  (<>) :: (tt vt) -> (tt vt) -> (tt vt)
  fv_set     :: (tt vt) -> Set.Set (tt vt)
  binder_set :: vt -> (tt vt) -> Set.Set (tt vt)

infixl 1 ~>
infixl 2 <>

data Term a = Var a
            | Abs a (Term a)
            | App (Term a) (Term a)
            deriving Show

instance LTerm VName Term where
  v = Var
  (~>) = Abs
  (<>) = App
  fv_set     = undefined
  binder_set = undefined

t1 = nx ~> vx <> (ny ~> vy <> vx)

nx = "x"
ny = "y"
nz = "z"
vx = v nx
vy = v ny
vz = v nz
