{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Service.HistoryLine 
    ( getHistoryLineR
    , handleGetHistoryLineR
    ) 
where

import Import
import Data.Acid.Advanced
import Text.Hamlet

import Audit.VersionedChange
import Db.AuditStore
import Util.EntityKey
import qualified Util.SequentialId as SeqId




getHistoryLineR ∷ EntityKey → Int → Handler Html
getHistoryLineR eKey howManyVersionsBack = handleGetHistoryLineR VersionR eKey howManyVersionsBack


handleGetHistoryLineR ∷ (AuditId → AppRoute) → EntityKey → Int 
                      → Handler Html
handleGetHistoryLineR versionUrl eKey howManyVersionsBack = do
                      ids ← listAudits eKey howManyVersionsBack
                      giveUrlRenderer $(hamletFile "templates/service/history-line.hamlet")


listAudits ∷ EntityKey → Int → Handler [AuditId]
listAudits k limit = do 
                   app ← getYesod
                   query' (db app) $ SelectHistoryLine limit k

