{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Util.EntityKey
    ( EntityKey
    , mkEntityKey
    , className
    , entityId
    , btmEntityKey
    , fromText
    , toText
    )
    where

import BaseImport
import Data.Data
import Data.List
import qualified Data.Text as T 
import Data.Text.Read


-- opaque id; mkEntityKey constructs valid values.
newtype EntityKey = EntityKey { unEntityKey ∷ (Text, Integer) } 
                  deriving (Eq, Ord, Read, Data, Typeable)

mkEntityKey ∷ Text → Integer → Maybe EntityKey
mkEntityKey clazzName entId 
            | entId > 0 = Just $ EntityKey (T.strip clazzName, entId)
            | otherwise = Nothing

className ∷ EntityKey → Text
className = fst ∘ unEntityKey

entityId ∷ EntityKey → Integer
entityId = snd ∘ unEntityKey

-- we only allow entityId > 0; so, if needed, this value could be used as ⊥
btmEntityKey ∷ EntityKey
btmEntityKey = EntityKey (T.empty, 0)  


instance Show EntityKey where
    show (EntityKey (c, i)) = T.unpack c ++ " # " ++ show i 


fromText ∷ Text → Maybe EntityKey
fromText = uncurry mkEntityKey ∘ toPair ∘ T.split (≡'~') ∘ T.strip
    where
    toPair xs | length xs > 1 = ( T.concat ∘ intersperse "~" ∘ init $ xs
                                , toIntegr ∘ last $ xs
                                )
              | otherwise     = (T.empty, 0)  -- will result in Nothing

toIntegr ∷ Text → Integer
toIntegr = either (const 0) check ∘ signed decimal ∘ T.strip
    where
    check (x, ts) = if T.null ts then x else 0


toText ∷ EntityKey → Text
toText k = T.concat [className k, "~", T.pack ∘ show ∘ entityId $ k]
