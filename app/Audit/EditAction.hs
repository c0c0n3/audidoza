{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Audit.EditAction 
    ( Edit(..)
    , EditAction
    , username
    , timeOfChange
    , contentId
    , auditedContent
    , newContent
    , modContent
    , delContent
    )
    where

import BaseImport
import Data.Data
import Data.Time


--
-- tracks changes to some identifiable content.
--  ⋅ new content = initial state when created
--  ⋅ mod content = subsequent changes (prev/cur = before/after change)
--  ⋅ del content = content disposed of
-- 
data Edit ξ = NewContent { newState  ∷ ξ } 
            | ModContent { prevState ∷ ξ, curState ∷ ξ }
            | DelContent { delState  ∷ ξ }
            deriving (Eq, Ord, Show, Data, Typeable)

--
-- content edit action as performed by some user at some point in time.
--  ⋅ content id = identifies content across versions 
--                 (e.g. class name + entity id)
--
data EditAction κ ξ = 
     EditAction { username       ∷ Text     -- who made the change
                , timeOfChange   ∷ UTCTime  -- when
                , contentId      ∷ κ        
                , auditedContent ∷ Edit ξ   -- actual content
                }
                deriving (Eq, Ord, Show, Data, Typeable)

newContent ∷ Text → UTCTime → κ → ξ → EditAction κ ξ
newContent user time contId content 
           = EditAction user time contId
           $ NewContent content

modContent ∷ Text → UTCTime → κ → ξ → ξ → EditAction κ ξ
modContent user time contId prevContent curContent 
           = EditAction user time contId 
           $ ModContent prevContent curContent

delContent ∷ Text → UTCTime → κ → ξ → EditAction κ ξ
delContent user time contId content 
           = EditAction user time contId
           $ DelContent content
