{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Test where

import Prelude.Unicode
import Data.Acid
import qualified Data.Text as Text

import Audit.EditAction
import Audit.ObjectHistory
import Db.AuditStore
import Util.EntityKey
import Util.Time

import TreeUtil
import XmlData



objKey ∷ EntityKey
objKey = k where Just k = mkEntityKey "root" 1

new_ usr xml = newContent usr epoc objKey (Text.pack xml)
mod_ usr xml₁ xml₂ = modContent usr epoc objKey (Text.pack xml₁) (Text.pack xml₂)
del_ usr xml = delContent usr epoc objKey (Text.pack xml)

uc₁ = new_ "u1" xt1
uc₂ = mod_ "u2" xt1 xt2
uc₃ = del_ "u2" xt2
uc₄ = new_ "u3" et1


acidTest ∷ IO ()
acidTest = withStore "audit-db" runTest

runTest ∷ AcidState ObjectHistory → IO ()
runTest db = do
{-
           update db $ WriteAudit uc₁
           update db $ WriteAudit uc₂
           update db $ WriteAudit uc₃
           update db $ WriteAudit uc₄
-}
           kcs ← query db $ AllEntityKeys
           print kcs

           putStrLn "*************************************************************"
           
           let ks  = map fst kcs
           hls ← mapM (query db ∘ SelectHistoryLine 100) ks 
           mapM_ print hls

           putStrLn "*************************************************************"
           
           vs ← query db $ SelectVersions (head hls)
           mapM_ print vs
