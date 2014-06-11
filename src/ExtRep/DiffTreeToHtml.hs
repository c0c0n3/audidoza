{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module ExtRep.DiffTreeToHtml --(toHtml5) where
where

import Prelude.Unicode
import Control.Monad
import Data.List
import Data.Ord
import Data.Tree.Class
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Diff.Content
import Diff.DiffTree



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

   <span class="unchanged">              (or new or deleted)
         <span class="nodeName">…</span>
         <span class="payload">…</span>
   </span>

for Unchanged, New, or Deleted diff node.  Changed diff node rendered as:

    <span class="changed">
          <span class="nodeName">…</span>
          <span class="payload">…</span>
          <span class="newPayload">…</span>
    </span>

-}
toHtml5 ∷ RenderableTree t ξ ⇒ DiffTree t ξ → Html
toHtml5 t = renderForest [t]

renderTree ∷ RenderableTree t ξ ⇒ DiffTree t ξ → Html
renderTree t = li (renderNode t >> renderForest (getChildren t))

renderForest ∷ RenderableTree t ξ ⇒ [DiffTree t ξ] → Html
renderForest [] = return ()
renderForest ts = ul $ mapM_ renderTree (sortBy cmp ts)
    where
    cmp = comparing (diffContentNodeId ∘ getNode)

renderNode ∷ RenderableTree t ξ ⇒ DiffTree t ξ → Html
renderNode = item ∘ getNode
    where
    item (New x)           = wrap "new" x
    item (Deleted x)       = wrap "deleted" x
    item (Unchanged x)     = wrap "unchanged" x
    item (Changed old new) = mkSpan "changed" $ 
                             (mkNameValueSpan old >> mkSpanH "newPayload" (payload new))
    
    mkSpan clazz = H.span ! class_ clazz
    mkSpanH clazz content = mkSpan clazz $ (toHtml content)
    mkNameValueSpan x = mkSpanH "nodeName" (nodeName x) >> mkSpanH "payload" (payload x)
    wrap clazz x = mkSpan clazz $ mkNameValueSpan x

