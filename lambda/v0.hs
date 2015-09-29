{-

\x.x x

["lam","x",["app",["var","x"],["var","x"]]]

-}

{- Nat -}
data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)

{- Combinators -}

cI = \x   -> x
cK = \x y -> x
--
cW = \f x   -> f x x
cC = \f x y -> f y x
--
cB = \f g x -> f (g x)
cS = \f g x -> f x (g x)

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

{- Expression -}

newtype Lit = Lit Int deriving Show

data Term = Abs (Lam ())
          | App (Lam ()) Term
          | Var Nat
          | Lift Lit
          deriving Show

newtype Lam a = Lam Term deriving Show

down :: Term -> Term
down (Abs (Lam t)) = Abs $ Lam (down t)
down (App (Lam t) t') = App (Lam $ down t) (down t')
down (Var (Succ n)) = Var n
down (Var Zero) = error ""
down (Lift l) = Lift l

subs :: Term -> Term -> Term
subs (Abs (Lam bodyTerm)) t = Abs $ Lam $ subs bodyTerm t
subs (App (Lam bodyTerm) argTerm) t = App (Lam $ subs bodyTerm t) argTerm
subs (Var Zero) t = t
subs (Var n) t = Var n
subs (Lift l) t = Lift l

sEval :: Term -> Term
sEval (Lift l) = Lift l
sEval (Var n) = Var n
sEval (Abs lt) = Abs lt
sEval (App (Lam (Abs (Lam bodyTerm))) argTerm) = sEval $ down $ subs bodyTerm argTerm

lift = Lift.Lit
fun = Abs.Lam

myI :: Term
myI = fun $ Var Zero
myK :: Term
myK = fun $ fun $ Var Zero

myW = -- \f x   -> f x x
myC = -- \f x y -> f y x
myB = -- \f g x -> f (g x)
myS = -- \f g x -> f x (g x)


s0 = \ s z -> z
s1 = \ s z -> s s0
s2 = \ s z -> s s1

s1 = \ s z -> s (\ s0 z0 -> z0)

eqZero n = n (\m -> false) true

   eqZero s1
=> (\n -> n (\m -> false) true) s1
=> s1 (\m -> false) true
=> (\ s z -> s (\ s0 z0 -> z0))
       (\m -> false)
       true
=> (\m -> false) (\ s0 z0 -> z0)
=> false


   eqZero s0
=> (\n -> n (\m -> false) true) s0
=> s0 (\m -> false) true
=> (\ s z -> z) (\m -> false) true
=> (\   z -> z) true
=> true















