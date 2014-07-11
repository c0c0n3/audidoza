{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Site.HistoryLine (getViewHistoryLineR) where

import Import

import Util.EntityKey
import Handler.Service.HistoryLine
import Handler.Site.Layout




getViewHistoryLineR ∷ EntityKey → Int → Handler Html
getViewHistoryLineR eKey howManyVersionsBack = do
                    semanticHtml ← handleGetHistoryLineR ViewVersionR eKey howManyVersionsBack
                    bareLayout $ do
                        setTitle "History Line"
                        toWidgetBody semanticHtml 
