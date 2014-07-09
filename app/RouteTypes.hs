{-# LANGUAGE UnicodeSyntax #-}
module RouteTypes where

import Yesod

import Util.EntityKey (EntityKey)
import qualified Util.EntityKey as EntityKey
import Util.SequentialId (SeqId)
import qualified Util.SequentialId as SeqId




instance PathPiece SeqId where
    toPathPiece   = SeqId.toText
    fromPathPiece = SeqId.fromText

instance PathPiece EntityKey where
    toPathPiece   = EntityKey.toText
    fromPathPiece = EntityKey.fromText
