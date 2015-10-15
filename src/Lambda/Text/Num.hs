module Lambda.Text.Num where

import Lambda.Text.Term
import Lambda.Text.NameSugar

data Nat = S Nat | Z deriving (Eq, Ord)

instance Show Nat where
  show = showNat 0

showNat :: Int -> Nat -> String
showNat i Z     = "."++(show i)
showNat i (S n) = showNat (i+1) n

-- Church
chZ = nf ~> nx ~> vx
chS = "n" ~> "s" ~> "z" ~> (Var "s") <> ((Var "n") <> (Var "s") <> (Var "z"))

ch0 = chZ     -- \ f x -> x
ch1 = chS <> ch0 -- \ f x -> f (ch0 f x) -- f x
ch2 = chS <> ch1 -- \ f x -> f (ch1 f x) -- f (f x)
ch3 = chS <> ch2 -- \ f x -> f (ch2 f x) -- f (f (f x))
ch4 = chS <> ch3
ch5 = chS <> ch4

(~+~) :: Term -> Term -> Term
m ~+~ n = m <> chS <> n

--Scott
scZ = \f x -> x
scS = \n f x -> f n

sc0 =     scZ
sc1 = scS sc0 -- f sc0
sc2 = scS sc1 -- f sc1
sc3 = scS sc2 -- f sc2
