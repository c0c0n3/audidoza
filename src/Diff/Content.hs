{-# LANGUAGE UnicodeSyntax, TypeFamilies, FlexibleContexts #-}
module Diff.Content where

import Prelude.Unicode



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
