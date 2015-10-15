{-# LANGUAGE PatternGuards #-}

module Haha.Lambda.NamedVar.BigLambdaEagerReduction
    ( betaRed
    ) where

import Haha.Lambda.NamedVar.Term

-- TODO:  beta-redex

betaRed :: Term -> Term
betaRed (Var y)   = (Var y)
betaRed (Abs y t) = Abs y (betaRed t)
betaRed (App p q) | Abs a t <- betaRed p
                  , red     <- betaRed q
                  , replaceable t red a
                  = betaRed $ replace t red a
betaRed (App p q) | Abs a t <- betaRed p
                  , red     <- betaRed q
                  , not $ replaceable t red a
                  = betaRed $ App (renaming (fv red) p) red
betaRed (App p q) | otherwise = App p (betaRed q)
