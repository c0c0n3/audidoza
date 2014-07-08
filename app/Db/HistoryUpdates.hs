{-# LANGUAGE UnicodeSyntax #-}
module Db.HistoryUpdates
    ( writeAudit
    )
    where

import BaseImport
import Control.Monad.State
import Data.Acid

import Audit.ContentHistory
import Audit.ObjectHistory




writeAudit ∷ ObjectEdit → Update ObjectHistory ()
writeAudit change = do 
                    db ← get
                    put $ pushChange change db
