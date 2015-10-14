module Haha.Lambda.NamedVar.Redex where

import Haha.Lambda.NamedVar.Term
import Haha.Lambda.NamedVar.NameSugar
import Haha.Lambda.NamedVar.InputSet

isRedex :: InputSet -> Term -> Bool
isRedex IS_Lambda (Var _)   = False
isRedex IS_Lambda (Abs a t) = isRedex IS_Lambda t
isRedex IS_Lambda (App p q) | Abs a t <- p = True
                                           || (isRedex IS_Lambda t)
                                           || (isRedex IS_Lambda q)
                            | otherwise    = (isRedex IS_Lambda p)
                                           || (isRedex IS_Lambda q)
--
isRedex IS_Gamma (Var _)   = False
isRedex IS_Gamma (Abs a t) = isRedex IS_Gamma t
isRedex IS_Gamma (App p q) | Abs a t <- p
                           , in_Gamma q
                           = True
--
isRedex IS_gamma (Var _)   = False
isRedex IS_gamma (Abs _ _) = False
isRedex IS_gamma (App p q) = undefined
