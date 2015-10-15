{-# LANGUAGE PatternGuards #-}

module Lambda.Text.Reduction
    ( rNF
    , rHNF
    , reduce1
    , isNF
    , isHNF
    ) where


import Lambda.Text.Term
import Lambda.Text.InputSet
import qualified Lambda.Text.Substitution as S

reduce2NF :: NormalForm -> InputSet -> Term -> Term
reduce2NF nf ips t = undefined

sequentialize :: InputSet -> Term -> [Term]
sequentialize ips t = seqize ips t []

seqize :: InputSet -> Term -> [Term] -> [Term]
seqize ips (Var a)   ap = undefined
seqize ips (Abs a t) ap = undefined
seqize ips (App p q) ap | (Var a)   <- p = undefined
seqize ips (App p q) ap | (Abs a t) <- p = undefined
seqize ips (App p q) ap | (App t u) <- p = undefined

{-
   f x y z
= (f x) y z
= ((f x) y) z
-}


-- ????????????????????????????
-- Q: not in Δ => is a Δ-redex
-- ????????????????????????????















{- ######################################### -}


rNF :: InputSet -> Term -> Maybe Term
rNF is (Var a)   = Just (Var a)
rNF is (Abs a t) | Just u <- rNF is t = if t == u
                    then Nothing else Just (Abs a u)
rNF is (Abs a t) | Nothing <- rNF is t = Nothing
rNF is (App p q) | Just s <- rNF is p
                 , Just t <- rNF is q
                 = if (s == p || t == q)
                     then Nothing
                     else if isRedex is (App s t)
                          then let u = reduce1 is (App s t) in
                              if u == (App s t)
                                  then Nothing
                                  else rNF is u
                          else Just (App s t)
rNF is (App p q) | otherwise
                 = Nothing

rHNF :: InputSet -> Term -> Maybe Term
rHNF = rNF

isNF :: InputSet -> Term -> Bool
isNF = undefined

isHNF :: InputSet -> Term -> Bool
isHNF = undefined

reduce1 :: InputSet -> Term -> Term
reduce1 is (Var a)   = Var a
reduce1 is (Abs a t) = Abs a (reduce1 is t)
--
reduce1 is (App p q) | not $ inInputSet is q
                     , isRedex is q
                     = App p (reduce1 is q)
reduce1 is (App p q) | not $ inInputSet is q
                     , not $ isRedex is q
                     , not $ isRedex is p
                     = (App p q)
--
reduce1 is (App p q) | not $ inInputSet is q
                     , not $ isRedex is q
                     , isRedex is p
                     , isHNF is p
                     = S.subs t q a where (Abs a t) = p
--
reduce1 is (App p q) | not $ isHNF is p
                     = reduce1 is (App p q)
--
