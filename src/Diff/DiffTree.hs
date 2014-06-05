{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
--
-- DiffTree = result of comparing previous version of content tree with current one.
-- comparison is as follows.  content nodes (tree labels) are compared by nodeId at
-- the same level of depth in the prev and curr tree to determine new, deleted and
-- matched nodes; payloads of each pair of matched content nodes are diff'ed to 
-- determine if the node has changed.
--
module Diff.DiffTree 
    ( DiffNode(..)
    , DiffTree
    , diff
    )
where

import Prelude.Unicode
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs

import Diff.Content
import Util.Match




data DiffNode ξ = New ξ | Deleted ξ | Unchanged ξ
                | Changed { oldNode ∷ ξ, newNode ∷ ξ }

instance Show ξ ⇒ Show (DiffNode ξ) where
    show (New n)       = "+ " ++ show n
    show (Deleted n)   = "- " ++ show n
    show (Unchanged n) = "= " ++ show n
    show (Changed m n) = "* " ++ show m ++ " |~~> " ++ show n 


type DiffTree ξ = NTree (DiffNode ξ)


diff ∷ (ContentNode ξ, Ord (Id ξ), Eq (Data ξ)) ⇒
       NTree ξ → NTree ξ → DiffTree ξ
diff previous current 
    | compareContentNode previous current ≡ EQ = diffMatched (previous, current)
    | otherwise                                = fmap New current


compareContentNode ∷ (ContentNode ξ, Ord (Id ξ)) ⇒ 
                     NTree ξ → NTree ξ → Ordering
compareContentNode t u = compare (nodeId t) (nodeId u)


splitNodes ∷ (ContentNode ξ, Ord (Id ξ)) ⇒ 
             [NTree ξ] → [NTree ξ] → ([DiffTree ξ], [(NTree ξ, NTree ξ)])
splitNodes previous current = (new ++ deleted, matched)
    where
    matchResult = matchBy compareContentNode previous current
    new         = map (fmap New)     $ rightOnly matchResult 
    deleted     = map (fmap Deleted) $ leftOnly  matchResult
    matched     = both matchResult


diffMatched ∷ (ContentNode ξ, Ord (Id ξ), Eq (Data ξ)) ⇒
              (NTree ξ, NTree ξ) → DiffTree ξ
diffMatched (previous, current) = NTree diffNode children
    where
    prevContent = getNode previous
    currContent = getNode current
    samePayload = payload prevContent ≡ payload currContent
    diffNode    = if samePayload 
                  then Unchanged prevContent 
                  else Changed prevContent currContent 
    children    = newOrDeleted ++ map diffMatched matched 
    (newOrDeleted, matched) = splitNodes (getChildren previous) (getChildren current)

