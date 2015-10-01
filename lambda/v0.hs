{-# LANGUAGE BangPatterns #-}

import Haha.Term

{-Church Number -}

type ChNum t = (t -> t) -> t -> t
--
ch0 :: ChNum t
ch0 = cK cI
ch1 :: ChNum t
ch1 = cS cB ch0
ch2 :: ChNum t
ch2 = cS cB ch1
ch3 :: ChNum t
ch3 = cS cB ch2
ch4 :: ChNum t
ch4 = cS cB ch3
--
ch :: Int -> ChNum t
ch 0 = cK cI
ch i = cS cB (ch (i-1))
--
chSucc :: ChNum t -> ChNum t
chSucc n = cS cB n
--

subs :: Term -> Term -> Term
subs Atom t = Atom
subs (Abs bodyTerm) t = Abs (subs bodyTerm t)
subs (App bodyTerm argTerm) t = App (subs bodyTerm t) (subs argTerm t)
subs (Var Zero) t = t
subs (Var (Succ n)) t = Var n

evalCbV :: Term -> Term
evalCbV Atom = Atom
evalCbV (Var n) = Var n
evalCbV (Abs lt) = Abs lt
evalCbV (App (Abs bodyT) argT) = let !t' = (evalCbV argT) in
    evalCbV $ subs bodyT t'
evalCbV (App bodyT argT) = evalCbV $ App (evalCbV bodyT) (evalCbV argT)

evalCbN :: Term -> Term
evalCbN Atom = Atom
evalCbN (Var n) = Var n
evalCbN (Abs lt) = Abs lt
evalCbN (App (Abs bodyT) argT) = evalCbN $ subs bodyT argT
evalCbN (App bodyT argT)       = evalCbN $ App (evalCbN bodyT) (evalCbN argT)


{- Combinators -}

cI = \x   -> x
cK = \x y -> x
cW = \f x   -> f x x
cC = \f x y -> f y x
cB = \f g x -> f (g x)
cS = \f g x -> f x (g x)

myI = fun v0
myK = fun (fun v0)
myW = fun $ fun $ (v0 <> v1) <> v1
myC = fun $ fun $ fun $ (v0 <> v2) <> v1
myB = fun $ fun $ fun $ v0 <> (v1 <> v2)
myS = fun $ fun $ fun $ (v0 <> v2) <> (v1 <> v2)

myo = fun (v0 <> v0)
myO = myo <> myo

main = do
    putStrLn $ pretty myS















