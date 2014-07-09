{-# LANGUAGE UnicodeSyntax #-}
module Util.Match
    ( match
    , matchBy
    , MatchResult
    , leftOnly
    , rightOnly
    , both
    )
where

import BaseImport
import Data.List




data MatchResult ξ = MatchResult 
                   { leftOnly  ∷ [ξ]
                   , rightOnly ∷ [ξ]
                   , both      ∷ [(ξ, ξ)] 
                   }
     deriving Show

addRight ∷ MatchResult ξ → [ξ] → MatchResult ξ
addRight r zs  = r { rightOnly = rightOnly r ++ zs }

addLeft ∷ MatchResult ξ → [ξ] → MatchResult ξ
addLeft r zs  = r { leftOnly = leftOnly r ++ zs }

addBoth ∷ MatchResult ξ → ξ → ξ → MatchResult ξ 
addBoth r x y = r { both = (x, y) : both r }


match ∷ Ord ξ ⇒ [ξ] → [ξ] → MatchResult ξ
match = matchBy compare

--
-- match left to right items using the ordering enforced by cmp.
-- return
--   ⋅ leftOnly:   x ∈ left s.t. x ∉ right
--   ⋅ rightOnly:  y ∈ right s.t. y ∉ left 
--   ⋅ both:       (x, y) s.t. x ∈ right, y ∈ left, x ≡ y
-- 
matchBy ∷ (ξ → ξ → Ordering) → [ξ] → [ξ] → MatchResult ξ
matchBy cmp left right = φ (MatchResult [] [] []) (sortBy cmp left) (sortBy cmp right)
    where
    φ r [] [] = r
    φ r [] ys = addRight r ys
    φ r xs [] = addLeft  r xs
    φ r lft@(x:xs) rgt@(y:ys) 
        | cmp x y ≡ EQ = φ (addBoth  r x y) xs  ys
        | cmp x y ≡ LT = φ (addLeft  r [x]) xs  rgt
        | cmp x y ≡ GT = φ (addRight r [y]) lft ys
    φ r _ _ = r
