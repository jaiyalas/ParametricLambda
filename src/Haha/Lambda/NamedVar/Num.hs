module Haha.Lambda.NamedVar.Num where

import Haha.Lambda.NamedVar.Term

data Nat = S Nat | Z deriving (Show,Eq)

-- Church
chZ = \f x -> x
chS = \n f x -> f (n f x)

ch0 = chZ     -- \ f x -> x
ch1 = chS ch0 -- \ f x -> f (ch0 f x) -- f x
ch2 = chS ch1 -- \ f x -> f (ch1 f x) -- f (f x)
ch3 = chS ch2 -- \ f x -> f (ch2 f x) -- f (f (f x))

--Scott
scZ = \f x -> x
scS = \n f x -> f n

sc0 =     scZ
sc1 = scS sc0 -- f sc0
sc2 = scS sc1 -- f sc1
sc3 = scS sc2 -- f sc2

-- sc2 (\m -> S (m (\n -> S (n S Z)) Z)) Z = S (S Z)

data NatX x = ZX | SX x deriving Show

data Fix f = In (f (Fix f))

type N = Fix NatX
