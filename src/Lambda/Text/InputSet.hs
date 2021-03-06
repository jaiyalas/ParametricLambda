module Lambda.Text.InputSet where

import Lambda.Text.Term
import Lambda.Text.NameSugar

-- Γ γ Ξ ξ Δ δ Λ λ
data InputSet = IS_Lambda
              | IS_Gamma
              | IS_gamma
              -- | IS_Lambda_I
              deriving (Ord,Eq)

instance Show InputSet where
    show IS_Lambda   = "Λ"
    show IS_Gamma    = "Γ"
    show IS_gamma    = "γ"
    -- show IS_Lambda_I = "ΛI"

in_Lambda :: Term -> Bool
in_Lambda = const True

in_Gamma :: Term -> Bool
in_Gamma (Var _)   = True
in_Gamma (Abs _ m) = in_Lambda m
in_Gamma _         = False

in_gamma :: Term -> Bool
in_gamma (Var _)   = True
in_gamma (Abs x t) = elem (Var x) (fv t)
in_gamma _         = False

inInputSet :: InputSet -> Term -> Bool
inInputSet IS_Lambda = in_Lambda
inInputSet IS_Gamma  = in_Gamma
inInputSet IS_gamma  = in_gamma

isRedex :: InputSet -> Term -> Bool
isRedex is (Var _)   = False
isRedex is (Abs p q) = False
isRedex is (App (Abs _ _) q) = inInputSet is q
isRedex is (App p         q) = False

data NormalForm = NF
                | HNF
                -- | WHNF
                deriving (Show, Eq, Ord)
