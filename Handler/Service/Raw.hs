{-# LANGUAGE UnicodeSyntax #-}
module Handler.Service.Raw
    ( getRawNewVersionR 
    , getRawPrevVersionR 
    , getRawCurVersionR 
    , getRawDelVersionR
    ) 
where

import Import

import Audit.ObjectHistory
import Audit.VersionedChange
import Handler.Service.Version




getRawNewVersionR ∷ AuditId → Handler Text
getRawNewVersionR audId = lookupContent getNewState audId

getRawPrevVersionR ∷ AuditId → Handler Text
getRawPrevVersionR audId = lookupContent getPrevState audId


getRawCurVersionR ∷ AuditId → Handler Text
getRawCurVersionR audId = lookupContent getCurState audId


getRawDelVersionR ∷ AuditId → Handler Text
getRawDelVersionR audId = lookupContent getDelState audId


lookupContent ∷ (VersionedObject → Maybe Text) → AuditId → Handler Text
lookupContent getter audId = do
              mv ← findVersion $ audId
              case (mv >>= getter) of
                   Just t  → return t 
                   Nothing → notFound -- NB 404 page is built from default template…
