{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Site.Raw
    ( getViewRawNewVersionR 
    , getViewRawPrevVersionR 
    , getViewRawCurVersionR 
    , getViewRawDelVersionR
    ) 
where

import Import
import Text.Hamlet

import Audit.VersionedChange
import Handler.Service.Raw
import Handler.Site.Layout




getViewRawNewVersionR ∷ AuditId → Handler Html
getViewRawNewVersionR audId = embedRawContent getRawNewVersionR audId


getViewRawPrevVersionR ∷ AuditId → Handler Html
getViewRawPrevVersionR audId = embedRawContent getRawPrevVersionR audId


getViewRawCurVersionR ∷ AuditId → Handler Html
getViewRawCurVersionR audId = embedRawContent getRawCurVersionR audId


getViewRawDelVersionR ∷ AuditId → Handler Html
getViewRawDelVersionR audId = embedRawContent getRawDelVersionR audId


embedRawContent ∷ (AuditId → Handler Text) → AuditId → Handler Html
embedRawContent getter audId = do
                rawContent ← getter audId
                bareLayout $ do
                    setTitle "Raw Object"
                    toWidgetBody $(hamletFile "templates/site/raw-audit-content.hamlet")
