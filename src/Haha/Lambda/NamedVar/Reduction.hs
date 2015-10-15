{-# LANGUAGE PatternGuards #-}

module Haha.Lambda.NamedVar.Reduction
    ( rNF
    , rHNF
    , reduce1
    , isNF
    , isHNF
    ) where


import Haha.Lambda.NamedVar.Term
import Haha.Lambda.NamedVar.InputSet
import Haha.Lambda.NamedVar.Redex

rNF :: InputSet -> Term -> Maybe Term
rNF is (Var a)   = Just (Var a)
rNF is (Abs a t) | Just u <- rNF is t = if t == u
                    then Nothing else Just (Abs a u)
rNF is (Abs a t) | Nothing <- rNF is t = Nothing
rNF is (App p q) | Just s <- rNF is p
                 | Just t <- rNF is q
                 = if (s == p || t == q)
                     then Nothing
                     else if isRedex is (App s t)
                          then if u == (App s t) then Nothing else rNF u
                              where u = reduce1 is (App s t)
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

reduce1 is (App p q) |


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
                     = subs t q a where (Abs a t) = p


reduce1 is (App p q) | not $ isHNF is p,
                     = reduce1 is (App p q)



--
reduce1 is (App p q) | isHNF is p
                     , (Abs a t) <- p
                     = subs t q a
