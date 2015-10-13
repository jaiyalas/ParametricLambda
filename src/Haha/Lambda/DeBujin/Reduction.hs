module Haha.Lambda.NamedVar.Reduction
   (
   InputSet(IS_Lambda, IS_Gamma, IS_gamma, IS_Lambda_I),
   inInputSet,
   in_Lambda,
   in_Gamma,
   in_gamma,
   in_Lambda_I,
   redWeak,
   red
   ) where

import qualified Data.Set as Set

import Haha.Lambda.NamedVar.Term
import Haha.Lambda.NamedVar.BigLambdaEagerReduction

-- Γ γ Ξ ξ Δ δ Λ λ

data InputSet = IS_Lambda
              | IS_Gamma
              | IS_gamma
              | IS_Lambda_I
              deriving (Show,Eq)

in_Lambda :: Term -> Bool
in_Lambda = const True

in_Gamma :: Term -> Bool
in_Gamma (Var _)   = True
in_Gamma (Abs _ _) = True
in_Gamma _         = False

in_gamma :: Term -> Bool
in_gamma (Var _)   = True
in_gamma (Abs x t) = elem (Var x) (fv t)
in_gamma _         = False

in_Lambda_I :: Term -> Bool
in_Lambda_I (Var _)   = True
in_Lambda_I (Abs x t) = elem (Var x) (fv t) && in_Lambda_I t
in_Lambda_I (App t u) = in_Lambda_I t && in_Lambda_I u

inInputSet :: InputSet -> Term -> Bool
inInputSet IS_Lambda   = in_Lambda
inInputSet IS_Gamma    = in_Gamma
inInputSet IS_gamma    = in_gamma
inInputSet IS_Lambda_I = in_Lambda_I

redWeak :: InputSet -> Term -> Term
redWeak is (Var x)     = (Var x)
redWeak is (Abs x tM)  = (Abs x tM)
redWeak is (App (Abs x tM) tN) = if (inInputSet is) tN
    then subs tM tN x
    else App (Abs x tM) tN

red :: InputSet -> Term -> Term
red is (App (Abs x tM) tN) = if (inInputSet is) tN
    then subs tM tN x
    else subs tM (red is tN) x
red _ others = others
