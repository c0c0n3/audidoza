{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Audit.ContentHistory
    ( ContentHistory(..)
    , HistoryLine
    , emptyContentHistory
    , historyLineKeys
    , historyLine
    , lookupVersions
    , currentVersion
    , countVersions
    , pushChange
    )
    where

import BaseImport
import qualified Control.Arrow as A
import Data.Data
import Data.IxSet.Typed
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Audit.EditAction
import Audit.VersionedChange
import Util.SequentialId
            



-- history line = all changes made to content identified by some content id 
--                (e.g. all changes made to object identified by given 
--                 class name + entity id)
newtype HistoryLine = HistoryLine { versions ∷ [AuditId] } deriving (Show, Data, Typeable) 
                      -- pointers to versioned changes in desc order
emptyHistoryLine ∷ HistoryLine
emptyHistoryLine = HistoryLine []

-- all audit records
data ContentHistory κ ξ = 
     ContentHistory { versionedChanges ∷ VersionedChanges κ ξ
                    , historyLines     ∷ Map κ HistoryLine
                    , lastAuditId      ∷ AuditId  -- id of last pushed change
                    }
                    deriving (Show, Typeable)

emptyContentHistory ∷ ContentHistory κ ξ
emptyContentHistory = ContentHistory empty Map.empty seqIdSeed

-- all content ids.
historyLineKeys ∷ Ord κ ⇒ ContentHistory κ ξ → [κ]
historyLineKeys = Map.keys ∘ historyLines

-- pointers to subsequent versions of content identified by κ, in desc order.
-- last change added through pushChange comes first in the list, second last
-- comes second, etc. 
historyLine ∷ Ord κ ⇒ κ → ContentHistory κ ξ → [AuditId]
historyLine contentKey = versions
                       ∘ Map.findWithDefault emptyHistoryLine contentKey 
                       ∘ historyLines

-- look up versioned changes given their audit ids.
-- returned list is sorted by audit id, so from most to least recent changes.
lookupVersions ∷ [AuditId] → ContentHistory κ ξ → [VersionedChange κ ξ]
lookupVersions ids = toLst ∘ (@+ (map AuditIdIx ids)) ∘ versionedChanges
    where
    toLst = toDescList (Proxy ∷ Proxy AuditIdIx)

-- version number and audit id of most recent version of content identified by κ.
currentVersion ∷ Ord κ ⇒ κ → ContentHistory κ ξ → Maybe (VersionId, AuditId)
currentVersion contentKey db = listToMaybe  -- safe head
                             ∘ map (version A.&&& auditId)
                             ∘ (flip lookupVersions db) 
                             ∘ take 1       -- latest (if any) is first
                             ∘ historyLine contentKey $ db

-- how many versions of the content identified by κ do we have?
countVersions ∷ Ord κ ⇒ κ → ContentHistory κ ξ → Integer
countVersions contentKey = fromMaybe 0 ∘ fmap (unSeqId ∘ fst) ∘ currentVersion contentKey
                         -- NB relying on versions being a stack and version number
                         -- being assigned incrementally

-- add content change to history, thus making new version of content identified 
-- by κ.
pushChange ∷ Ord κ ⇒ EditAction κ ξ → ContentHistory κ ξ → ContentHistory κ ξ
pushChange chnge db = ContentHistory 
                      { versionedChanges = insert vc ∘ versionedChanges $ db
                      , historyLines = pushVersion vc ∘ historyLines $ db
                      , lastAuditId = auditId vc 
                      }
    where
    vc = newVersion db chnge

-- push audit id of new version into versions stack.
pushVersion ∷ Ord κ ⇒ VersionedChange κ ξ → Map κ HistoryLine → Map κ HistoryLine
pushVersion newVers = Map.insertWith ifExists kontentId initialEntry
    where
    kontentId    = contentId ∘ editAction $ newVers
    newAuditId   = auditId newVers
    initialEntry = HistoryLine [newAuditId]
    ifExists _ (HistoryLine stack) = HistoryLine (newAuditId : stack)

-- make new versioned change for given edit action.
-- (entails assigning next audit id and version number.)
newVersion ∷ Ord κ ⇒ ContentHistory κ ξ → EditAction κ ξ → VersionedChange κ ξ
newVersion db chnge = 
           VersionedChange
           { auditId    = succ ∘ lastAuditId $ db
           , version    = succ ∘ fromMaybe seqIdSeed ∘ fmap fst 
                          ∘ currentVersion (contentId chnge) $ db
           , editAction = chnge
           }
