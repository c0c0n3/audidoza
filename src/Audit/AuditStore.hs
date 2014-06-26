{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Audit.AuditStore
    ( withAudit
    , writeAuditChange
    , queryObject
    , fetchObjectVersion
    )
    where

import Prelude.Unicode
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
--import Control.Applicative
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
--import Data.SafeCopy
--import Data.Typeable
import System.IO

import Audit.AcidTypes
import Audit.History
import Audit.ObjectHistory
import Diff.ObjectTree




withAudit ∷ FilePath → (AcidState ObjectHistory → IO ξ) → IO ξ
withAudit acidStoreDir = bracket (openStore acidStoreDir) (createCheckpointAndClose)

openStore ∷ FilePath → IO (AcidState ObjectHistory)
openStore acidStoreDir = openLocalStateFrom acidStoreDir emptyChangeHistory

writeAuditChange ∷ ObjectNode → UserChange ObjectAudit 
                   → Update ObjectHistory ()
writeAuditChange historyLineKey change = do 
                                         db ← get
                                         put $ auditChange db historyLineKey change

queryObject ∷ ObjectNode → (UserChange ObjectAudit → Bool) 
              → Query ObjectHistory [VersionedObject]
queryObject historyLineKey query = do
                                   db ← ask
                                   return $ selectObject db historyLineKey query

fetchObjectVersion ∷ ObjectNode → Integer 
                     → Query ObjectHistory (Maybe VersionedObject)
fetchObjectVersion historyLineKey version = do
                                            db ← ask
                                            return $ selectObjectVersion db historyLineKey version
{-
TODO: add queryObjects ?
-- query versions across all instances of a class
selectObjects ∷ ObjectHistory → Text → (UserChange ObjectAudit → Bool)
                → [(ObjectNode, [VersionedChange ObjectAudit])]
selectObjects h className = selectFromHistory h ((≡className) ∘ nodeName)
-}

{-
TODO
-- $(makeAcidic ''ObjectHistory ['storeAuditChange, 'queryObject])
$(makeAcidic ''ObjectHistory ['writeAuditChange, 'fetchObjectVersion])
-}

instance IsAcidic ObjectHistory where
    acidEvents = [ UpdateEvent (\(WriteAuditChange k c) → writeAuditChange k c)
                 , QueryEvent (\(QueryObject k q) → queryObject k q)
                 , QueryEvent (\(FetchObjectVersion k v) → fetchObjectVersion k v)
                 ]
