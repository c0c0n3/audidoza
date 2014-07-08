{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Service.HistoryLine (getHistoryLineR) where

import Import
import Data.Acid.Advanced

import Audit.VersionedChange
import Db.AuditStore
import Util.EntityKey
import qualified Util.SequentialId as SeqId




getHistoryLineR ∷ EntityKey → Int → Handler Html
getHistoryLineR entityKey howManyVersionsBack = do
                ids ← listAudits entityKey howManyVersionsBack
                defaultLayout $ renderAudits ids


listAudits ∷ EntityKey → Int → Handler [AuditId]
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
