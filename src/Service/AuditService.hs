{-# LANGUAGE UnicodeSyntax #-}
module Service.AuditService where

import Prelude.Unicode
import Data.Acid
import Yesod

import Audit.ObjectHistory




data AuditService = AuditService { db ∷ AcidState ObjectHistory }

type Handlr = HandlerT AuditService IO

