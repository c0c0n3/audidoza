{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Service.Keys 
    ( getKeysR
    , handleGetKeysR
    ) 
where

import Import
import Data.Acid.Advanced
import Data.List
import Text.Hamlet

import Db.AuditStore
import Db.HistoryQueries (VersionsCount)
import Util.EntityKey




getKeysR ∷ Handler Html
getKeysR = handleGetKeysR HistoryLineR


handleGetKeysR ∷ (EntityKey → Int → AppRoute) → Handler Html
handleGetKeysR historyLineUrl = do
               ksv ← listEntityKeys
               giveUrlRenderer $(hamletFile "templates/service/keys.hamlet")


listEntityKeys ∷ Handler [(EntityKey, VersionsCount)]
listEntityKeys = do 
               app ← getYesod
               ks  ← query' (db app) AllEntityKeys
               return ∘ sort $ ks
