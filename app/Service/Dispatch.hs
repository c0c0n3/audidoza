{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Service.Dispatch where

import Prelude.Unicode
import Data.Text (Text)
import Yesod

import Audit.VersionedChange
import Service.AuditService
import Service.Audits
import Service.HistoryLine
import Service.Keys
import Service.Routes
import Service.Version



-- TODO: duplicated routes declaration--see Routes; move both to external file.
mkYesodDispatch "AuditService" [parseRoutes|

/audits AuditsR POST
/audits/keys KeysR GET
/audits/versions/entity/#EntityKey/last/#Integer HistoryLineR GET
/audits/versions/id/#AuditId VersionR GET

|]

