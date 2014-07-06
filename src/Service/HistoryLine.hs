{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
module Service.HistoryLine (getHistoryLineR) where

import Prelude.Unicode
import Data.Acid.Advanced
import Yesod

import Audit.VersionedChange
import Db.AuditStore
import Service.AuditService
import Service.Routes
import Util.EntityKey
import qualified Util.SequentialId as SeqId




getHistoryLineR ∷ EntityKey → Int → Handlr Html
getHistoryLineR entityKey howManyVersionsBack = do
                ids ← listAudits entityKey howManyVersionsBack
                defaultLayout $ renderAudits ids


listAudits ∷ EntityKey → Int → Handlr [AuditId]
listAudits k limit = do 
                   app ← getYesod
                   query' (db app) $ SelectHistoryLine limit k


renderAudits ids = [whamlet|
    <div class="auditsListView">
      <ul class="auditsList">
        $forall i <- ids
          <li class="audit">
            <a href=@{VersionR i}
               title="View version.">version: #{SeqId.toText i}
|]
