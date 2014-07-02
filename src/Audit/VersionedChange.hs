{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable, DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
module Audit.VersionedChange
    ( AuditId
    , VersionId
    , VersionedChange(..)
    , AuditIdIx(..)
    , UsernameIx(..)
    , TimeOfChangeIx(..)
    , AuditIxs
    , VersionedChanges
    )
    where

import Prelude.Unicode
import Data.Data
import Data.IxSet.Typed
import Data.Text (Text)
import Data.Time (UTCTime)

import Audit.EditAction
import Util.SequentialId




type AuditId   = SeqId
type VersionId = SeqId

-- audit record
--  ⋅ version = incremental identifier to track subsequent changes to content
--              identified by κ (i.e. content id; so it's unique within the 
--              the scope of κ, but not across versioned changes for ≢ κ)
--
data VersionedChange κ ξ = 
     VersionedChange { auditId    ∷ AuditId   -- unique across versioned changes
                     , version    ∷ VersionId
                     , editAction ∷ EditAction κ ξ
                     }
                     deriving (Show, Data, Typeable)

instance Eq (VersionedChange κ ξ) where
    v == w = auditId v ≡ auditId w   

instance Ord (VersionedChange κ ξ) where
    compare v w = compare (auditId v) (auditId w)


newtype AuditIdIx = AuditIdIx AuditId deriving (Eq, Ord, Data, Typeable)
newtype UsernameIx = UsernameIx Text deriving (Eq, Ord, Data, Typeable)
newtype TimeOfChangeIx = TimeOfChangeIx UTCTime deriving (Eq, Ord, Data, Typeable)

type AuditIxs = '[AuditIdIx, UsernameIx, TimeOfChangeIx]

type VersionedChanges κ ξ = IxSet AuditIxs (VersionedChange κ ξ)


instance Indexable AuditIxs (VersionedChange κ ξ) where
    empty = mkEmpty
            (mkIx AuditIdIx auditId)
            (mkIx UsernameIx (username ∘ editAction))
            (mkIx TimeOfChangeIx (timeOfChange ∘ editAction))
        where
        mkIx ctor accesor = ixFun $ (:[]) ∘ ctor ∘ accesor
