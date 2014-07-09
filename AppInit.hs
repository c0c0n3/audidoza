{-# LANGUAGE UnicodeSyntax #-}
module AppInit where

import Prelude
import Prelude.Unicode
import Control.Concurrent
import qualified Data.Text as Text
import Yesod.Default.Config

import Db.AuditStore
import Settings



initAuditStore ∷ AppConfig DefaultEnv Extra → IO AuditStore
initAuditStore config = do
               auditStore ← openStore $ storeDir config
               _ ← forkIO $ checkpointLoop auditStore $ interval config
               return auditStore

checkpointLoop ∷ AuditStore → Int → IO ()
checkpointLoop db seconds = do
               threadDelay $ seconds * 1000000
               checkpoint db
               checkpointLoop db seconds

storeDir ∷ AppConfig DefaultEnv Extra → String
storeDir = Text.unpack ∘ auditStoreDir ∘ appExtra

interval ∷ AppConfig DefaultEnv Extra → Int
interval = checkpointInterval ∘ appExtra
