{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Diff.Content where

import Prelude.Unicode
import Data.Text (Text)
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

    nodeId   ∷ Ord (Id ξ) ⇒ ξ → Id ξ
    nodeName ∷ ξ → Text
    payload  ∷ Eq (Data ξ) ⇒ ξ → Data ξ

--
-- convenience: direct access to label from node
--
instance (ContentNode ξ, Tree t) ⇒ ContentNode (t ξ) where

    type Id (t ξ)   = Id ξ
    type Data (t ξ) = Data ξ

    nodeId   = nodeId   ∘ getNode
    nodeName = nodeName ∘ getNode
    payload  = payload  ∘ getNode

--
-- interface for a tree made up of content nodes
--
class (Functor t, Tree t, ContentNode ξ, Ord (Id ξ), Eq (Data ξ)) 
      ⇒ ContentTree t ξ
