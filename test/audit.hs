{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Test where

import Prelude.Unicode
import Data.Maybe
import qualified Data.Text as Text

import Audit.History
import Audit.ObjectHistory
import Util.Time

import TreeUtil
import XmlData




objKey = mkObjectHistoryLineKey "root" 1
addChange history username xml = auditChange history objKey 
                               $ userChange username epoc (Text.pack xml) 


h₁ = addChange emptyChangeHistory "u1" xt1
h₂ = addChange h₁ "u2" xt2
history = addChange h₂ "u3" et1

c1 = describeChange history objKey 1
c2 = describeChange history objKey 2
c3 = describeChange history objKey 3

showDelta = printNTree ∘ snd ∘ fromJust

show_d1 = showDelta c1
show_d2 = showDelta c2
show_d3 = showDelta c3


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
