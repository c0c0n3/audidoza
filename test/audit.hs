{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Test where

import Prelude.Unicode
import Data.Acid
import Data.Maybe
import qualified Data.Text as Text

import Audit.AcidTypes
import Audit.AuditStore
import Audit.History
import Audit.ObjectHistory
import Util.Time

import TreeUtil
import XmlData




objKey = mkObjectHistoryLineKey "root" 1

new_ usr xml = userChange usr epoc $ NewObject (Text.pack xml)
mod_ usr xml₁ xml₂ = userChange usr epoc $ ModObject (Text.pack xml₁) (Text.pack xml₂)
del_ usr xml = userChange usr epoc $ DelObject (Text.pack xml)

uc₁ = new_ "u1" xt1
uc₂ = mod_ "u2" xt1 xt2
uc₃ = del_ "u2" xt2
uc₄ = new_ "u3" et1

logChg history = auditChange history objKey
h₁ = logChg emptyChangeHistory uc₁
h₂ = logChg h₁ uc₂
h₃ = logChg h₂ uc₃
history = logChg h₃ uc₄

showDelta = printNTree ∘ fromJust ∘ fmap describeChangeTree ∘ selectObjectVersion history objKey
show_d1 = showDelta 1
show_d2 = showDelta 2
show_d3 = showDelta 3
show_d4 = showDelta 4


{-
e ∷ ChangeHistory String String
e = emptyChangeHistory

h₁ = auditChange e  "k1" (UserChange "u1" "k1 c1")
h₂ = auditChange h₁ "k1" (UserChange "u2" "k1 c2")
h₃ = auditChange h₂ "k2" (UserChange "u2" "k2 d1")
h₄ = auditChange h₃ "k2" (UserChange "u3" "k2 d2")
h₅ = auditChange h₄ "k1" (UserChange "u3" "k1 c3")

select = selectFromHistoryLine h₅ -- "k1" ((≡"u1") ∘ username)
                                  -- "k1" ((≡"k1") ∘ take 2 ∘ auditedContent)
xselect = selectFromHistory h₅ -- (\k → k ≡ "k1" ∨ k ≡ "k2") ((≡"u2") ∘ username)

h₅ ⇝ ChangeHistory {contentHistory = fromList 
[("k1",[V 3, C "u3" "k1 c3",
        V 2, C "u2" "k1 c2",
        V 1, C "u1" "k1 c1"]),
 ("k2",[V 2, C "u3" "k2 d2",
        V 1, C "u2" "k2 d1"])
]}
-}

acidTest ∷ IO ()
acidTest = withAudit "audit-db" runTest
         
runTest ∷ AcidState ObjectHistory → IO ()
runTest db = do
           vs ← query db $ QueryObject objKey ((≡"u2") ∘ username)
           print vs
{-
           update db $ WriteAuditChange objKey uc₁
           update db $ WriteAuditChange objKey uc₂
           update db $ WriteAuditChange objKey uc₃
           update db $ WriteAuditChange objKey uc₄
           
           Just v₁ ← query db $ FetchObjectVersion objKey 1
           Just v₂ ← query db $ FetchObjectVersion objKey 2
           Just v₃ ← query db $ FetchObjectVersion objKey 3
           Just v₄ ← query db $ FetchObjectVersion objKey 4
           
           printNTree ∘ describeChangeTree $ v₁
           printNTree ∘ describeChangeTree $ v₂
           printNTree ∘ describeChangeTree $ v₃
           printNTree ∘ describeChangeTree $ v₄
-}
