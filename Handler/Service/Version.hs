{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Service.Version (getVersionR) where

import Import
import Data.Acid.Advanced
import Data.Maybe
import Text.Hamlet

import Audit.EditAction
import Audit.ObjectHistory
import Audit.VersionedChange
import Db.AuditStore
import ExtRep.DiffTreeToHtml ()
import Util.SequentialId
import Util.Time





getVersionR ∷ AuditId → Handler Html
getVersionR audId = do
                    mv ← findVersion audId
                    case mv of
                         Just v  → giveUrlRenderer 
                                   $(hamletFile "templates/service/version.hamlet")
                         Nothing → notFound -- NB 404 page is built from default template…  


findVersion ∷ AuditId → Handler (Maybe VersionedObject)
findVersion audId = do 
                    app ← getYesod
                    v   ← query' (db app) $ SelectVersions [audId]
                    return ∘ listToMaybe $ v 


renderDiff ∷ VersionedObject → Html
renderDiff = toHtml ∘ describeChangeTree ∘ editAction

