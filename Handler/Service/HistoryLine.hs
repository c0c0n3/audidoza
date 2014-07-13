{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Service.HistoryLine 
    ( Urls(..)
    , getHistoryLineR
    , handleGetHistoryLineR
    ) 
where

import Import
import Data.Acid.Advanced
import Text.Hamlet

import Audit.EditAction
import Audit.ObjectHistory
import Audit.VersionedChange
import Db.AuditStore
import Util.EntityKey
import qualified Util.SequentialId as SeqId
import Util.Time




data Urls = Urls 
          { versionUrl ∷ AuditId → AppRoute
          , rawNewUrl  ∷ AuditId → AppRoute
          , rawPrevUrl ∷ AuditId → AppRoute
          , rawCurUrl  ∷ AuditId → AppRoute
          , rawDelUrl  ∷ AuditId → AppRoute
          }


getHistoryLineR ∷ EntityKey → Int → Handler Html
getHistoryLineR eKey howManyVersionsBack = handleGetHistoryLineR u eKey howManyVersionsBack
    where
    u = Urls VersionR RawNewVersionR RawPrevVersionR RawCurVersionR RawDelVersionR


handleGetHistoryLineR ∷ Urls → EntityKey → Int → Handler Html
handleGetHistoryLineR u eKey howManyVersionsBack = do
                      versions ← listAudits eKey howManyVersionsBack
                      giveUrlRenderer $(hamletFile "templates/service/history-line.hamlet")


listAudits ∷ EntityKey → Int → Handler [VersionedObject]
listAudits k limit = do 
                   app ← getYesod
                   ids ← query' (db app) $ SelectHistoryLine limit k
                   query' (db app) $ SelectVersions ids
