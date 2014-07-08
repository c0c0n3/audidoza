{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds, TypeSynonymInstances, FlexibleInstances #-}
--
-- implements ToMarkup for diff tree so that you can call Blaze toHtml on a
-- diff tree.
--
module ExtRep.DiffTreeToHtml (RenderableTree) where

import BaseImport
import Control.Monad
import Data.List
import Data.Ord
import Data.Tree.Class
import Text.Blaze (toValue)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Diff.Content
import Diff.DiffTree


-- convenience constraint synonym
type RenderableTree t ξ = (ContentTree t ξ, ToMarkup (Data ξ))

{- 
produce an HTML ul structure to mirror input tree, e.g.

    ul
      li RootClass
        ul
          li field₁
          li field₂
          li ClassX
            ul
              li f₁
              li ClassY
                ul
                  li f₁

li text = rendered content node; rendering as follows:

   <span class="unchanged inner">     (or new or deleted; inner|leaf to indicate node depth)
         <span class="nodeName">…</span>
         <span class="payload">…</span>
   </span>

for Unchanged, New, or Deleted diff node.  Changed diff node rendered as:

    <span class="changed leaf">          (or inner)
          <span class="nodeName">…</span>
          <span class="payload">…</span>
          <span class="newPayload">…</span>
    </span>

-}
instance RenderableTree t ξ ⇒ ToMarkup (DiffTree t ξ) where
    toMarkup t = renderForest [t]  -- NB Blaze toHtml function is an alias for this:
                                   -- type Html = Markup, toHtml = toMarkup

renderTree ∷ RenderableTree t ξ ⇒ DiffTree t ξ → Html
renderTree t = li (renderNode t >> renderForest (getChildren t))

renderForest ∷ RenderableTree t ξ ⇒ [DiffTree t ξ] → Html
renderForest [] = return ()
renderForest ts = ul $ mapM_ renderTree (sortBy cmp ts)
    where
    cmp = comparing (diffContentNodeId ∘ getNode)

renderNode ∷ RenderableTree t ξ ⇒ DiffTree t ξ → Html
renderNode t = item ∘ getNode $ t
    where
    nic ∷ String → AttributeValue   -- node indicator class
    nic clazz = toValue $ (clazz++) $ if isLeaf t then " leaf" else " inner"  
    
    item (New x)           = wrap (nic "new") x
    item (Deleted x)       = wrap (nic "deleted") x
    item (Unchanged x)     = wrap (nic "unchanged") x
    item (Changed old new) = mkSpan (nic "changed") $ 
                             (mkNameValueSpan old >> mkSpanH "newPayload" (payload new))

    mkSpan clazz = H.span ! class_ clazz
    mkSpanH clazz content = mkSpan clazz $ (toHtml content)
    mkNameValueSpan x = mkSpanH "nodeName" (nodeName x) >> mkSpanH "payload" (payload x)
    wrap clazz x = mkSpan clazz $ mkNameValueSpan x
