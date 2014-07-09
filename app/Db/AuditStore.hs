{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Db.AuditStore where

import BaseImport
import Control.Exception
import Data.Acid
import Data.Acid.Local

import Audit.ContentHistory
import Audit.ObjectHistory
import Db.AcidTypes ()
import Db.HistoryQueries
import Db.HistoryUpdates




type AuditStore = AcidState ObjectHistory

openStore ∷ FilePath → IO AuditStore
openStore acidStoreDir = openLocalStateFrom acidStoreDir emptyContentHistory

withStore ∷ FilePath → (AuditStore → IO ξ) → IO ξ
withStore acidStoreDir = bracket (openStore acidStoreDir) closeStore

closeStore ∷ AuditStore → IO ()
closeStore = createCheckpointAndClose


$(makeAcidic ''ObjectHistory 
             [ 'writeAudit
             , 'allEntityKeys
             , 'selectHistoryLine
             , 'selectVersions
             ])
