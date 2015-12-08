module Simple where

open import Data.Nat renaming (_≟_ to _≟ℕ_)
open import Data.Fin hiding (_+_; inject)
open import Data.String hiding (_++_) renaming (_≟_ to _≟S_)
open import Data.List
open import Data.Bool

FName : Set
FName = String

data Var₀ : Set where
   v : FName → Var₀

data Term₀ : Set where
   v₀  : (vx : Var₀) → Term₀
   λ₀  : (x : FName) → (t : Term₀) → Term₀
   _·_ : (e₁ : Term₀) → (e₂ : Term₀) → Term₀

fv : Term₀ → List Var₀
fv (v₀ vx) = vx ∷ []
fv (λ₀ x t) = filter {!   !} (fv t)
fv (t · t₁) = {!   !}
