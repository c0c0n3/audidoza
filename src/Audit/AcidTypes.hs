{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, StandaloneDeriving, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Audit.AcidTypes where

import Prelude.Unicode
import Control.Applicative
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Typeable

import Audit.History
import Audit.ObjectHistory
import Diff.ObjectTree




deriving instance Typeable ObjectNode
deriving instance Typeable ObjectAudit
deriving instance Typeable VersionedChange
deriving instance Typeable ChangeHistory

$(deriveSafeCopy 0 'base ''ObjectNode)
$(deriveSafeCopy 0 'base ''UserChange)
$(deriveSafeCopy 0 'base ''VersionedChange)
$(deriveSafeCopy 0 'base ''ObjectAudit)


instance SafeCopy ObjectHistory where
    putCopy = contain ∘ safePut ∘ historyLines 
    getCopy = contain $ changeHistory <$> safeGet


{-
writeAuditChange ∷ ObjectNode → UserChange ObjectAudit 
                   → Update ObjectHistory ()
-}
data WriteAuditChange = WriteAuditChange ObjectNode (UserChange ObjectAudit)
deriving instance Typeable WriteAuditChange

instance SafeCopy WriteAuditChange where
    putCopy (WriteAuditChange on uc) = contain $ safePut on >> safePut uc
    getCopy = contain $ WriteAuditChange <$> safeGet <*> safeGet 

instance Method WriteAuditChange where
    type MethodResult WriteAuditChange = ()
    type MethodState WriteAuditChange = ObjectHistory

instance UpdateEvent WriteAuditChange


{-
queryObject ∷ ObjectNode → (UserChange ObjectAudit → Bool) 
              → Query ObjectHistory [VersionedObject]
-}
data QueryObject = QueryObject ObjectNode (UserChange ObjectAudit → Bool)
deriving instance Typeable QueryObject

instance SafeCopy QueryObject where
    putCopy _ = contain $ return ()
    getCopy = contain ∘ return $ QueryObject (mkObjectHistoryLineKey "" 0) (const False)

instance Method QueryObject where
    type MethodResult QueryObject = [VersionedObject]
    type MethodState QueryObject = ObjectHistory

instance QueryEvent QueryObject


{-
fetchObjectVersion ∷ ObjectNode → Integer 
                     → Query ObjectHistory (Maybe VersionedObject)
-}
data FetchObjectVersion = FetchObjectVersion ObjectNode Integer
deriving instance Typeable FetchObjectVersion

instance SafeCopy FetchObjectVersion where
    putCopy _ = contain $ return ()
    getCopy = contain ∘ return $ FetchObjectVersion (mkObjectHistoryLineKey "" 0) 0

instance Method FetchObjectVersion where
    type MethodResult FetchObjectVersion = Maybe VersionedObject
    type MethodState FetchObjectVersion = ObjectHistory

instance QueryEvent FetchObjectVersion

