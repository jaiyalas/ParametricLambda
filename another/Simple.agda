module Simple where

open import Data.Nat renaming (_≟_ to _≟ℕ_)
open import Data.Fin hiding (_+_; inject)
open import Data.String hiding (_++_) renaming (_≟_ to _≟S_)
open import Relation.Nullary using (¬_; yes; no)
open import Relation.Binary.PropositionalEquality using (refl; _≡_)

open import Data.List

open import Data.Sum using (_⊎_; inj₁; inj₂)

FName : Set
FName = String

FNames : Set
FNames = List FName

data Expr : ℕ → Set where
   num : ∀ {n} → ℕ → Expr n
   bv  : ∀ {n} → (i : Fin n) → Expr n
   fv  : ∀ {n} → (x : FName) → Expr n
   ƛ   : ∀ {n} → (e : Expr (suc n)) → Expr n
   _·_ : ∀ {n} → (f : Expr n) → (e : Expr n) → Expr n

-- This idx indicates the amount of existence of lambda
-- out of current express

Expr0 : Set
Expr0 = Expr zero

↓ℕ≠ℕ : ∀ {n m} {i : Fin m}
     → ¬ (suc n ≡ toℕ (suc i))
     → ¬ (n ≡ toℕ i)
↓ℕ≠ℕ {n} {m} {i} sn≠si n≡i rewrite n≡i = sn≠si refl
-- ↓ℕ≠ℕ {n} {m} {i} sn≠si n≡i with n≡i
-- ↓ℕ≠ℕ sn≠si n≡i | refl = sn≠si refl

↓fin : ∀ {n} → (i : Fin (suc n)) → ¬ (n ≡ toℕ i) → Fin n
↓fin {zero} zero 0≠0 with 0≠0 refl
↓fin {zero} zero 0≠0 | ()
↓fin {zero} (suc ()) 0≠n
↓fin {suc n} zero i≠0 = zero
↓fin {suc n} (suc i) sn≠si = suc (↓fin i (↓ℕ≠ℕ sn≠si))

↑expr : ∀ {n} → Expr n → Expr (suc n)
↑expr (num i) = num i
↑expr (bv i)   = bv (inject₁ i)
-- inject₁ : ∀ {m} → Fin m → Fin (suc m)
↑expr (fv x)   = fv x
↑expr (ƛ e)    = ƛ (↑expr e)
↑expr (e · e₁) = ↑expr e · ↑expr e₁

{- substitution for bounded and free variable-}

[_↦_] : ∀ n → Expr n → Expr (suc n) → Expr n
[ m ↦ t ] (num i) = num i
[ m ↦ t ] (bv i) with m ≟ℕ toℕ i
... | yes m=i = t
... | no  m≠i = bv (↓fin i m≠i)
[ m ↦ t ] (fv x) = fv x
[ m ↦ t ] (ƛ e) = ƛ ([ suc m ↦ ↑expr t ] e)
[ m ↦ t ] (e · e₁) = [ m ↦ t ] e · [ m ↦ t ] e₁

[_↤_] : ∀ n → FName → Expr n → Expr (suc n)
[ m ↤ name ] (num x)  = num x
[ m ↤ name ] (bv i)   = ↑expr (bv i)
[ m ↤ name ] (fv x)   with x ≟S name
[ m ↤ name ] (fv x) | yes p = bv (fromℕ m)
[ m ↤ name ] (fv x) | no ¬p = fv x
[ m ↤ name ] (ƛ t)    = ƛ ([ suc m ↤ name ] t)
[ m ↤ name ] (t · t₁) = [ m ↤ name ] t · [ m ↤ name ] t₁

[_↝_] : ∀ {n} → FName → Expr n → Expr n → Expr n
[ n ↝ t ] (num i) = num i
[ n ↝ t ] (bv i) = bv i
[ n ↝ t ] (fv x) with n ≟S x
[ n ↝ t ] (fv x) | yes p = t
[ n ↝ t ] (fv x) | no ¬p = fv x
[ n ↝ t ] (ƛ x) = ƛ ([ n ↝ ↑expr t ] x)
[ n ↝ t ] (x · y) = [ n ↝ t ] x · [ n ↝ t ] y

_₀↦_ : Expr 1 → Expr 0 → Expr 0
m ₀↦ t = [ 0 ↦ t ] m


_↦₀_ : FName → Expr 0 → Expr 1
name ↦₀ t = [ 0 ↤ name ] t

_₀↤_ : Expr 0 → FName → Expr 1
t ₀↤ x = [ 0 ↤ x ] t

_₀↝_ : Expr 1 → FName → Expr 0
x ₀↝ s = x ₀↦ (fv s)

fvars : ∀ {n} → Expr n → FNames
fvars (num x) = []
fvars (bv i)  = []
fvars (fv x)  = x ∷ []
fvars (ƛ f)   = fvars f
fvars (f · x) = fvars f ++ fvars x

-- ############################## --
{-         locally closed         -}
-- ############################## --

open import Data.List.Any as Any
open Any.Membership-≡ using (_∈_; _∉_)
open import Data.Product

data LC : ∀ {n} → Expr n → Set where
   numᶜ : ∀ {n} → (i : ℕ) → LC {n} (num i)
   fvᶜ  : ∀ {n}
       → (x : FName)
       → LC {n} (fv x)
   _·ᶜ_ : ∀ {n} {f x}
       → LC {n} f
       → LC {n} x
       → LC {n} (f · x)
   ƛᶜ   : ∀ {e}
       → (ns : FNames)
       → ( ∀ {x} → x ∉ ns → LC {0} (e ₀↦ fv x) )
       → LC {0} (ƛ e)

postulate fresh-gen      : FNames → FName
postulate fresh-gen-spec : ∀ ns → fresh-gen ns ∉ ns

genName : (ns : FNames) → ∃ (λ x → x ∉ ns)
genName ns = fresh-gen ns , fresh-gen-spec ns

orz : ∀ {e} {nn} → nn ↦₀ (e ₀↦ fv nn) ≡ e
orz = {!   !}

foo : ∀ {nn} → (e : Expr 1) → LC {0} (e ₀↦ fv nn) → LC {1} e
foo (num i) lcp = {!   !}
foo (bv i)  lcp = {!   !}
foo (fv y)  lcp = {!   !}
foo (ƛ e)   lcp = {!   !}
foo (f · x) lcp = {!   !}

absᶜ  : ∀ {e} → LC {0} (ƛ e) → LC {1} e
absᶜ (ƛᶜ ns lcex) = {!   !}
   where nn    = proj₁ (genName ns)
         nn∉ns = proj₂ (genName ns)
         x     = lcex {nn} nn∉ns

appᶜ₁ : ∀ {n} {f x} → LC {n} (f · x) → LC {n} f
appᶜ₁ (lcf ·ᶜ lcx) = lcf

appᶜ₂ : ∀ {n} {f x} → LC {n} (f · x) → LC {n} x
appᶜ₂ (lcf ·ᶜ lcx) = lcx

lc? : ∀ {n} → (e : Expr n) → (LC {n} e ⊎ ¬ LC {n} e)
lc? (num x) = inj₁ (numᶜ x)
lc? (bv i)  = inj₂ (λ ())
lc? (fv x)  = inj₁ (fvᶜ x)
lc? (ƛ e)   with lc? e
lc? (ƛ e) | inj₁ x = inj₁ {!   !}
lc? (ƛ e) | inj₂ y = inj₂ {!   !}
lc? (f · x) with lc? f | lc? x
lc? (f · x) | inj₁ f' | inj₁ x' = inj₁ (f' ·ᶜ x')
lc? (f · x) | inj₁ f' | inj₂ x' = inj₂ (λ p → x' (appᶜ₂ p))
lc? (f · x) | inj₂ f' | _       = inj₂ (λ p → f' (appᶜ₁ p))

-- ############################## --
{-       value and semantics      -}
-- ############################## --

data Val : Expr0 → Set where
   num⁰ : ∀ n → Val (num n)
   ƛ⁰   : ∀ e → Val (ƛ e)

var? : (e : Expr0) → (Val e ⊎ ¬ (Val e))
var? (num i) = inj₁ (num⁰ i)
var? (bv i) = inj₂ (λ ())
var? (fv x) = inj₂ (λ ())
var? (ƛ x) = inj₁ (ƛ⁰ x)
var? (x · y) = inj₂ (λ ())

-- don't know what the hell this shit is
data _⟼_ : Expr 0 → Expr 0 → Set where
   app : ∀ {body para}
       → ((ƛ body) · para) ⟼ (body ₀↦ para)
