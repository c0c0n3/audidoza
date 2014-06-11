{-# LANGUAGE UnicodeSyntax #-} 
{-# LANGUAGE FlexibleContexts #-}
--
-- DiffTree = result of comparing previous version of content tree with current one.
-- comparison is as follows.  content nodes (tree labels) are compared by nodeId at
-- the same level of depth in the prev and curr tree to determine new, deleted and
-- matched nodes; payloads of each pair of matched content nodes are diff'ed to 
-- determine if the node has changed.
--
module Diff.DiffTree 
    ( DiffNode(..)
    , diffContentNodeId
    , DiffTree
    , diff
    )
where

import Prelude.Unicode
import Data.Ord
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

diffContentNodeId ∷ (ContentNode ξ, Ord (Id ξ)) ⇒ DiffNode ξ → Id ξ
diffContentNodeId (New x)       = nodeId x
diffContentNodeId (Deleted x)   = nodeId x
diffContentNodeId (Unchanged x) = nodeId x
diffContentNodeId (Changed x _) = nodeId x


type DiffTree t ξ = t (DiffNode ξ)


diff ∷ ContentTree t ξ ⇒ t ξ → t ξ → DiffTree t ξ
diff previous current 
    | nodeId previous ≡ nodeId current = diffMatched (previous, current)
    | otherwise                        = fmap New current


splitNodes ∷ ContentTree t ξ ⇒ [t ξ] → [t ξ] → ([DiffTree t ξ], [(t ξ, t ξ)])
splitNodes previous current = (new ++ deleted, matched)
    where
    matchResult = matchBy (comparing nodeId) previous current
    new         = map (fmap New)     $ rightOnly matchResult 
    deleted     = map (fmap Deleted) $ leftOnly  matchResult
    matched     = both matchResult


diffMatched ∷ ContentTree t ξ ⇒ (t ξ, t ξ) → DiffTree t ξ
diffMatched (previous, current) = mkTree diffNode children
    where
    prevContent = getNode previous
    currContent = getNode current
    samePayload = payload prevContent ≡ payload currContent
    diffNode    = if samePayload 
                  then Unchanged prevContent 
                  else Changed prevContent currContent 
    children    = newOrDeleted ++ map diffMatched matched 
    (newOrDeleted, matched) = splitNodes (getChildren previous) (getChildren current)

