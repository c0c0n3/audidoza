{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Service.Routes where

import Prelude.Unicode
import Data.Text (Text)
import Yesod

import Audit.VersionedChange
import Service.AuditService
import Util.EntityKey (EntityKey)
import qualified Util.EntityKey as EntityKey
import Util.SequentialId (SeqId)
import qualified Util.SequentialId as SeqId



-- TODO: duplicated routes declaration--see Dispatch; move both to external file.
mkYesodData "AuditService" [parseRoutes|

/audits AuditsR POST
/audits/keys KeysR GET
/audits/versions/entity/#EntityKey/last/#Int HistoryLineR GET
/audits/versions/id/#AuditId VersionR GET

|]

instance Yesod AuditService

instance PathPiece SeqId where
    toPathPiece   = SeqId.toText
    fromPathPiece = SeqId.fromText

instance PathPiece EntityKey where
    toPathPiece   = EntityKey.toText
    fromPathPiece = EntityKey.fromText
