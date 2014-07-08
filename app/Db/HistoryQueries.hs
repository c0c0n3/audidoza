{-# LANGUAGE UnicodeSyntax #-}
module Db.HistoryQueries
    ( VersionsCount  
    , allEntityKeys
    , selectHistoryLine
    , selectVersions
    )
    where

import BaseImport
import Control.Monad.Reader
import Data.Acid

import Audit.ContentHistory
import Audit.ObjectHistory
import Audit.VersionedChange
import Util.EntityKey




type VersionsCount = Integer
allEntityKeys ∷ Query ObjectHistory [(EntityKey, VersionsCount)]
allEntityKeys = do
                db ← ask
                return $ map (count db) ∘ historyLineKeys $ db
    where
    count db k = (k, countVersions k db)


selectHistoryLine ∷ Int → EntityKey → Query ObjectHistory [AuditId]
selectHistoryLine limit entityKey = 
                  do
                  db ← ask
                  return $ take limit ∘ historyLine entityKey $ db


selectVersions ∷ [AuditId] → Query ObjectHistory [VersionedObject]
selectVersions ids = do
                     db ← ask
                     return $ lookupVersions ids db

