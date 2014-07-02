{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Test where

import Prelude.Unicode
import qualified Data.Text as Text

import Audit.ContentHistory
import Audit.EditAction
import Audit.ObjectHistory
import Audit.VersionedChange
import Diff.ObjectTree
import Util.Time

import TreeUtil
import XmlData


objKey ∷ ObjectNode
objKey = mkEntityKey "root" 1

new_ usr xml = newContent usr epoc objKey (Text.pack xml)
mod_ usr xml₁ xml₂ = modContent usr epoc objKey (Text.pack xml₁) (Text.pack xml₂)
del_ usr xml = delContent usr epoc objKey (Text.pack xml)

history ∷ ObjectHistory
history = foldl (flip pushChange) emptyContentHistory
        [ new_ "u1" xt1      -- 1st change audited
        , mod_ "u2" xt1 xt2  -- 2nd
        , del_ "u2" xt2      -- 3rd
        , new_ "u3" et1      -- 4th
        ]

[v₄, v₃, v₂, v₁] = (flip lookupVersions history) ∘ historyLine objKey $ history

show_v1 = print v₁
show_v2 = print v₂
show_v3 = print v₃
show_v4 = print v₄
show_cv = print $ currentVersion objKey history
show_vn = print $ countVersions objKey history

showDelta = printNTree ∘ describeChangeTree ∘ editAction
show_d1 = showDelta v₁
show_d2 = showDelta v₂
show_d3 = showDelta v₃
show_d4 = showDelta v₄

