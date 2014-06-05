{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Diff.Content where

import Prelude.Unicode
import Data.Tree.Class


--
-- interface for labels of content tree.
-- content tree = tree whose labels are ContentNode's
-- nodeId gives the means to match labels
-- payload returns the content that is to be diff'ed
--
class ContentNode ξ where
    type Id ξ
    type Data ξ
    
    nodeId  ∷ Ord (Id ξ)  ⇒ ξ → Id ξ
    payload ∷ Eq (Data ξ) ⇒ ξ → Data ξ

--
-- convenience: direct access to label from node
--
instance (ContentNode ξ, Tree t) ⇒ ContentNode (t ξ) where

    type Id (t ξ)   = Id ξ
    type Data (t ξ) = Data ξ

    nodeId  = nodeId  ∘ getNode
    payload = payload ∘ getNode
