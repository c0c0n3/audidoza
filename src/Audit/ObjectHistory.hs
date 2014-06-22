{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Audit.ObjectHistory
    ( AuditTree
    , AuditTreeDelta
    , ObjectAudit
    , ObjectHistory
    , mkObjectHistoryLineKey
    , selectObject
    , selectObjects
    , auditChange
    , describeChange
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

{-
ASSUMPTIONS
≡≡≡≡≡≡≡≡≡≡≡

In Billdoza reads and writes to an object (tree) by a user are serialized.
Specifically, 

    s₀ → [u₁| ρ=s₀ ω₁] → s₁ = ω₁(s₀) → [u₂| ρ=s₁ ω₂] → s₂ = ω₂(s₁) → …

    user u₁ reads object state s₀, writes new state s₁;
    user u₂ reads object state s₁, writes new state s₂;
    …

but never interleaved r/w actions, e.g.

    s₀ → [u₁| ρ=s₀] → [u₂| ρ=s₀] → [u₂| ω₂] → s₁ = ω₂(s₀) → [u₁| ω₁] → s₂ = ω₁(s₀)
    
    u₂ update is lost!

Also, Billdoza calls to the audit service are serialized and happen in the exact
same order as the object writes:
     
     [α=(u₁,c,s₁)] → h₁ = (u₁,c,s₁) → [α=(u₂,c,s₂)] → h₂ = (u₂,c,s₂) → …

     Billdoza audits state change s₁ (s₁ instance of class c) made by u₁;
     history version 1 is created;
     Billdoza audits state change s₂ (s₂ instance of class c) made by u₂;
     history version 2 is created;
     …

Under these assumptions, what a user changed in version n is simply given by:

    diff h(n-1) h(n)

NOTE
≡≡≡≡
Another approach would be to require Billdoza to send both the state that was
initially read and the one that was written by a user.  In that case, we'd
only need to diff the two statuses and would always know what a user changed.

-}



type AuditTree      = ObjectTree NTree
type AuditTreeDelta = DiffTree NTree ObjectNode
type ObjectAudit    = Text
type ObjectHistory  = ChangeHistory ObjectNode ObjectAudit


mkObjectHistoryLineKey ∷ Text → Integer → ObjectNode
mkObjectHistoryLineKey className entityId = getNode rootOnlyTree 
    where
    rootOnlyTree = object className entityId [] ∷ AuditTree

-- query versions of a specific object
selectObject ∷ ObjectHistory → ObjectNode → (UserChange ObjectAudit → Bool)
               → [VersionedChange ObjectAudit]
selectObject = selectFromHistoryLine

-- query versions across all instances of a class
selectObjects ∷ ObjectHistory → Text → (UserChange ObjectAudit → Bool)
                → [(ObjectNode, [VersionedChange ObjectAudit])]
selectObjects h className = selectFromHistory h ((≡className) ∘ nodeName)

-- add new object (tree) state to its history line 
auditChange ∷ ObjectHistory → ObjectNode → UserChange ObjectAudit → ObjectHistory
auditChange = pushChange

-- diff requested version with previous. (note: if initial version requested, 
-- then tree delta will have all nodes marked as new.)
-- if requested version doesn't exist, Nothing returned.
describeChange ∷ ObjectHistory → ObjectNode → Integer
                 → Maybe (VersionedChange ObjectAudit, AuditTreeDelta)
describeChange h historyLineKey version = fmap (\vc → (vc, treeDelta)) $ requested
    where
    requested = selectVersion h historyLineKey version
    previous  = selectVersion h historyLineKey (version - 1)
    treeDelta = diff (auditedTree previous) (auditedTree requested)

auditedTree ∷ Maybe (VersionedChange ObjectAudit) → AuditTree
auditedTree = fromText ∘ fromMaybe Text.empty ∘ fmap (auditedContent ∘ changeItem)

fromText ∷ Text → AuditTree
fromText = fromXml ∘ toXTree 
