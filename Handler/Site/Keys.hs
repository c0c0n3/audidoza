{-# LANGUAGE UnicodeSyntax #-}
module Handler.Site.Keys (getViewKeysR) where

import Import

import Handler.Service.Keys
import Handler.Site.Layout




getViewKeysR ∷ Handler Html
getViewKeysR = do
             semanticHtml ← handleGetKeysR ViewHistoryLineR
             bareLayout $ do
                        setTitle "All History Line Keys"
                        toWidgetBody semanticHtml 
