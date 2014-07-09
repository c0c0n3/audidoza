{-# LANGUAGE UnicodeSyntax #-}
module Util.List
    ( projectIndexes
    )
where

import BaseImport
import Data.List



--
-- projects elements of input list at specified indexes.
-- input list is indexed starting from 0 on (i.e. x₀, x₁, etc.)
-- e.g.
--      projectIndexes [] []           ⇝ []
--      projectIndexes [1] []          ⇝ []
--      projectIndexes [] [0]          ⇝ []
--      projectIndexes [0] [0]         ⇝ [0]
--      projectIndexes [] [0..9]       ⇝ []
--      projectIndexes [1] [0..9]      ⇝ [1]
--      projectIndexes [1,0] [0..9]    ⇝ [0,1]
--      projectIndexes [1,0,-1] [0..9] ⇝ [0,1]
--      projectIndexes [1..6] [0..9]   ⇝ [1,2,3,4,5,6]
--
projectIndexes ∷ [Integer] → [α] → [α]
projectIndexes indexes xs = project (sort indexes) (zip [0..] xs)
    where
    project ixs@(i:is) zs@((j,x):rest)
            | i < j = project is zs
            | i ≡ j = x : project is rest
            | i > j = project ixs rest
    project _ _ = []
