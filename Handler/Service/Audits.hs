{-# LANGUAGE UnicodeSyntax #-}
module Handler.Service.Audits (postAuditsR) where

import Import
import Data.Acid.Advanced
import Data.Conduit
import Data.Conduit.List 
import Data.Conduit.Text
import qualified Data.Text as Text

import Db.AuditStore
import ExtRep.XmlToObjectEdit




postAuditsR ∷ Handler ()
postAuditsR = do
            xml ← readRequestBody
            let editAction = fromEither ∘ parseObjectEdit $ xml
            app ← getYesod
            update' (db app) $ WriteAudit editAction

-- NB reads the whole thing into memory, voiding benefits of streaming…
-- also, may throw an exception if UTF-8 decoding not successful.
readRequestBody ∷ MonadHandler m ⇒ m Text
readRequestBody = (rawRequestBody $= decodeUtf8 $$ consume) 
                >>= (return ∘ Text.concat)

fromEither (Right v)       = v
fromEither (Left errorMsg) = error $ Text.unpack errorMsg  
                             -- TODO: come up w/ better solution, e.g. throw/log
