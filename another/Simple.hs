{-# LANGUAGE OverloadedStrings #-}
--
module Simple where
--
type Name = String
newtype Var = Var Name deriving (Show, Eq, Ord)
data Term = Variable Var
          | LamIntro Name Term
          | LamElimi Term Term
          deriving (Show, Eq, Ord)
--
var :: Name -> Term
var n = Variable $ Var n
fun :: Name -> Term -> Term
fun = LamIntro
(.$.) :: Term -> Term -> Term
(.$.) = LamElimi
--
fv :: Term -> [Var]
fv (Variable v) = return v
fv (LamIntro n t) = filter (/= (Var n)) $ fv t
fv (LamElimi t u) = foldr excJoin (fv u) (fv t)
   where excJoin x h = if elem x h then h else x:h
--
isFFI :: Term -> Var -> Term -> Bool
isFFI m vx (Variable v) = True
isFFI m (Var xn) (LamIntro yn t) = xn /= yn
                                  && notElem (Var yn) (fv m)
                                  && isFFI m (Var xn) t
isFFI m vx (LamElimi t u) = (isFFI m vx t) && (isFFI m vx u)
--
unsafeRep :: Term -> Var -> Term -> Term
unsafeRep (Variable (Var yn)) (Var xn) t | yn == xn = t
unsafeRep (Variable (Var yn)) (Var xn) t | yn /= xn = (Variable (Var yn))
unsafeRep (LamIntro yn t1) (Var xn) t | yn == xn = LamIntro yn t1
unsafeRep (LamIntro yn t1) (Var xn) t | yn /= xn =
   LamIntro yn (unsafeRep t1 (Var xn) t)
unsafeRep (LamElimi t1 t2) (Var xn) t =
   LamElimi (unsafeRep t1 (Var xn) t) (unsafeRep t2 (Var xn) t)
--
replace :: Term -> Var -> Term -> Maybe Term
replace m (Var xn) t | isFFI m (Var xn) t = Just $ unsafeRep m (Var xn) t
replace m (Var xn) t | otherwise = Nothing
--
convA :: Term -> Var -> Term
convA (LamIntro xn m) (Var yn) | isFFI (var yn) (Var xn) m
                               , notElem (Var yn) (fv m)
                               = LamIntro yn $ unsafeRep m (Var xn) $ Variable $ Var yn
convA t _ = t
