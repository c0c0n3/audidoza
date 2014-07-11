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
-- TODO: DiffTreeCss includes "noise.png", i.e. no type-safe URL.
--       One option would be to base-64 encode the image and include it as data in CSS;
--       another option would be to pass a type-safe URL to DiffTreeCss.main.
