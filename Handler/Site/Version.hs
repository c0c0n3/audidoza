{-# LANGUAGE UnicodeSyntax #-}
module Handler.Site.Version (getViewVersionR) where

import Import

import Audit.VersionedChange
import Handler.Service.Version
import Handler.Site.Layout




getViewVersionR ∷ AuditId → Handler Html
getViewVersionR audId = do            
                semanticHtml ← getVersionR audId
                bareLayout $ do
                           setTitle "Diff Tree"
                           addStylesheet $ StaticR css_diff_tree_css
                           toWidgetBody semanticHtml 

-- TODO: add Shake build file to generate CSS and *then* build the site.

