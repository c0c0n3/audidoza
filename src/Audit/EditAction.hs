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

import Prelude.Unicode
import Data.Data
import Data.Text (Text)
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
newContent user time contentId content 
           = EditAction user time contentId
           $ NewContent content

modContent ∷ Text → UTCTime → κ → ξ → ξ → EditAction κ ξ
modContent user time contentId prevContent curContent 
           = EditAction user time contentId 
           $ ModContent prevContent curContent

delContent ∷ Text → UTCTime → κ → ξ → EditAction κ ξ
delContent user time contentId content 
           = EditAction user time contentId
           $ DelContent content
