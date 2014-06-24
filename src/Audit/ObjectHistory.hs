{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Audit.ObjectHistory
    ( ObjectAudit(..)
    , VersionedObject
    , ObjectHistory
    , AuditTree
    , AuditTreeDelta
    , mkObjectHistoryLineKey
    , auditChange
    , selectObject
    , selectObjects
    , selectObjectVersion
    , describeChangeTree
    )
    where

import Prelude.Unicode
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs

import Audit.History
import Diff.Content
import Diff.DiffTree
import Diff.ObjectTree
import ExtRep.XmlToObjectTree
import Util.Hxt




data ObjectAudit = NewObject { newState  ∷ Text } 
                 | ModObject { prevState ∷ Text, curState ∷ Text }
                 | DelObject { delState  ∷ Text }

type VersionedObject = VersionedChange ObjectAudit
type ObjectHistory   = ChangeHistory ObjectNode ObjectAudit
type AuditTree       = ObjectTree NTree
type AuditTreeDelta  = DiffTree NTree ObjectNode


mkObjectHistoryLineKey ∷ Text → Integer → ObjectNode
mkObjectHistoryLineKey className entityId = getNode rootOnlyTree 
    where
    rootOnlyTree = object className entityId [] ∷ AuditTree

-- add new object (tree) state to its history line 
auditChange ∷ ObjectHistory → ObjectNode → UserChange ObjectAudit → ObjectHistory
auditChange = pushChange

-- query versions of a specific object
selectObject ∷ ObjectHistory → ObjectNode → (UserChange ObjectAudit → Bool)
               → [VersionedObject]
selectObject = selectFromHistoryLine

-- query versions across all instances of a class
selectObjects ∷ ObjectHistory → Text → (UserChange ObjectAudit → Bool)
                → [(ObjectNode, [VersionedObject])]
selectObjects h className = selectFromHistory h ((≡className) ∘ nodeName)

-- retrieve specific version of an object
selectObjectVersion ∷ ObjectHistory → ObjectNode → Integer 
                      → Maybe VersionedObject
selectObjectVersion = selectVersion

-- diff previous object tree with current.
-- note: if new object, tree delta will have all nodes marked as new;
--       if deleted object, tree delta will be empty.
describeChangeTree ∷ VersionedObject → AuditTreeDelta
describeChangeTree = uncurry diff ∘ prevAndCurrentTrees ∘ auditedContent ∘ changeItem 
    where
    prevAndCurrentTrees (NewObject x)     = (fromText "", fromText x)
    prevAndCurrentTrees (ModObject x₁ x₂) = (fromText x₁, fromText x₁)
    prevAndCurrentTrees (DelObject x)     = (fromText x, fromText "")

fromText ∷ Text → AuditTree
fromText = fromXml ∘ toXTree 
