{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Db.AcidTypes where

import BaseImport
import Control.Applicative
import Data.Data
import Data.SafeCopy

import Audit.ContentHistory
import Audit.EditAction
import Audit.ObjectHistory
import Audit.VersionedChange
import Util.EntityKey
import Util.SequentialId




$(deriveSafeCopy 0 'base ''SeqId)
$(deriveSafeCopy 0 'base ''EntityKey)
$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''EditAction)
$(deriveSafeCopy 0 'base ''VersionedChange)
$(deriveSafeCopy 0 'base ''HistoryLine)
-- $(deriveSafeCopy 0 'base ''ContentHistory)  doesn't work, need manual deriving below

instance SafeCopy ObjectHistory where

    putCopy h = contain $
                 safePut (versionedChanges h)
              >> safePut (historyLines h)
              >> safePut (lastAuditId h)
    
    getCopy = contain $ ContentHistory <$> safeGet <*> safeGet <*> safeGet
