{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Util.SequentialId
    ( SeqId
    , unSeqId
    , seqIdSeed
    , fromText
    , toText
    )
    where

import BaseImport hiding (null)
import Data.Data
import Data.Either
import Data.Text
import Data.Text.Read


-- opaque id; succ function gives next id from old.
newtype SeqId = SeqId { unSeqId ∷ Integer } 
                deriving (Eq, Ord, Enum, Read, Show, Data, Typeable)

seqIdSeed = SeqId 0  -- we only allow to generate values from 1 on
                     -- if needed, this value could be used as ⊥

fromText ∷ Text → Maybe SeqId
fromText = check ∘ either (const (0, empty)) id ∘ signed decimal ∘ strip
    where
    check (x, ts) | null ts ∧ x > 0  = Just ∘ SeqId $ x
                  | otherwise        = Nothing

toText ∷ SeqId → Text
toText = pack ∘ show ∘ unSeqId
