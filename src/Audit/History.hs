{-# LANGUAGE UnicodeSyntax #-}
module Audit.History 
    ( UserChange
    , userChange
    , username
    , timeOfChange
    , auditedContent
    , VersionedChange
    , versionNumber
    , changeItem
    , ChangeHistory
    , changeHistory
    , historyLines
    , emptyChangeHistory
    , pushChange
    , historyLine
    , selectVersion
    , selectFromHistoryLine
    , selectFromHistory
    )
    where

import Prelude.Unicode
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Data.Time




data UserChange γ = 
     UserChange { username       ∷ Text
                , timeOfChange   ∷ UTCTime
                , auditedContent ∷ γ
                }
                deriving Show

userChange ∷ Text → UTCTime → γ → UserChange γ
userChange = UserChange 

data VersionedChange γ = 
     VersionedChange { versionNumber ∷ Integer 
                     , changeItem    ∷ UserChange γ
                     }
                     deriving Show
--
-- history line   = subsequent changes to content of type κ; each change gets an
--                  increasing version number, scoped by κ.
-- change history = collection of history lines, each of a different content type.
-- 
newtype ChangeHistory κ γ = 
        ChangeHistory { historyLines ∷ Map κ [VersionedChange γ]
                      }
                      deriving Show

changeHistory ∷ Ord κ ⇒ Map κ [VersionedChange γ] → ChangeHistory κ γ
changeHistory = ChangeHistory

emptyChangeHistory ∷ ChangeHistory κ γ
emptyChangeHistory = ChangeHistory Map.empty


pushChange ∷ Ord κ ⇒ ChangeHistory κ γ → κ → UserChange γ → ChangeHistory κ γ
pushChange (ChangeHistory h) contentKey change = 
            ChangeHistory $ Map.insertWith push contentKey initialEntry h
    where
    initialEntry        = [VersionedChange 1 change]
    push _ []           = initialEntry
    push _ stack@(v:vs) = VersionedChange (1 + versionNumber v) change : stack

historyLine ∷ Ord κ ⇒ ChangeHistory κ γ → κ → [VersionedChange γ]
historyLine (ChangeHistory h) contentKey = concat ∘ maybeToList ∘ Map.lookup contentKey $ h

selectVersion ∷ Ord κ ⇒ ChangeHistory κ γ → κ → Integer → Maybe (VersionedChange γ)
selectVersion h contentKey version = listToMaybe 
                                   ∘ filter ((≡version) ∘ versionNumber) 
                                   $ historyLine h contentKey

selectFromHistoryLine ∷ Ord κ ⇒ ChangeHistory κ γ → κ → (UserChange γ → Bool) 
                         → [VersionedChange γ]
selectFromHistoryLine h contentKey query = filter (query ∘ changeItem) 
                                         $ historyLine h contentKey

selectFromHistory ∷ Ord κ ⇒ ChangeHistory κ γ → (κ → Bool) → (UserChange γ → Bool) 
                         → [(κ, [VersionedChange γ])]
selectFromHistory (ChangeHistory h) keySelector query = Map.foldrWithKey φ [] h
    where
    φ k vs kvs | keySelector k ∧ (not ∘ null $ vs') = (k, vs') : kvs
               | otherwise     = kvs
        where
        vs' = filter (query ∘ changeItem) vs

