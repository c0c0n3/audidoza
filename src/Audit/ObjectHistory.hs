{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Audit.ObjectHistory
    ( ObjectEdit
    , VersionedObject
    , ObjectHistory
    , AuditTreeDelta
    , describeChangeTree
    )
    where

import Prelude.Unicode
import Data.Text (Text)
import Data.Tree.NTree.TypeDefs

import Audit.EditAction
import Audit.ContentHistory
import Audit.VersionedChange
import Diff.DiffTree
import Diff.ObjectTree
import ExtRep.XmlToObjectTree
import Util.Hxt




type ObjectEdit      = EditAction ObjectNode Text
type VersionedObject = VersionedChange ObjectNode Text
type ObjectHistory   = ContentHistory ObjectNode Text

type AuditTree       = ObjectTree NTree
type AuditTreeDelta  = DiffTree NTree ObjectNode

--
-- diff previous object tree with current.
-- note: if new object, tree delta will have all nodes marked as new;
--       if deleted object, tree delta will be empty.
--
describeChangeTree ∷ ObjectEdit → AuditTreeDelta
describeChangeTree = uncurry diff ∘ prevAndCurrentTrees ∘ auditedContent
    where
    prevAndCurrentTrees (NewContent x)     = (fromText "", fromText x)
    prevAndCurrentTrees (ModContent x₁ x₂) = (fromText x₁, fromText x₂)
    prevAndCurrentTrees (DelContent x)     = (fromText x, fromText "")

fromText ∷ Text → AuditTree
fromText = fromXml ∘ toXTree 
