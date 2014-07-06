{-# LANGUAGE UnicodeSyntax #-}
module Service.Runner (run) where

import Prelude.Unicode
import Data.Acid
import Yesod

import Audit.ObjectHistory
import Db.AuditStore
import Service.AuditService
import Service.Dispatch



run ∷ IO ()
run = withStore "audit-db" runServer  
    -- TODO read path/to/db from config file

runServer ∷ AcidState ObjectHistory → IO ()                
runServer db = warp 3000 AuditService { db = db }
             -- TODO read port from config file
