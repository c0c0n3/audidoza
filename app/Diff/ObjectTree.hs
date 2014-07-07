{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
--
-- ObjectTree = tree representation of input xml 
-- input xml  = xml representation of object tree that was saved in Billdoza
--
module Diff.ObjectTree 
    ( ObjectNode
    , ObjectTree
    , field
    , object
    , addChildren
    )
where

import Prelude.Unicode
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class

import Diff.Content
import Util.EntityKey




data ObjectNode = Field  { fieldName ∷ Text, fieldValue ∷ Text }
                | Object EntityKey

instance Show ObjectNode where
    show (Field n v) = Text.unpack n ++ " : " ++ Text.unpack v
    show (Object k)  = show k 

instance Eq ObjectNode where
    (Field m _) == (Field n _) = m ≡ n
    (Object h)  == (Object k)  = h ≡ k
    _            == _          = False

instance Ord ObjectNode where
    compare (Field m _) (Field n _) = compare m n
    compare (Field _ _) (Object _)  = LT
    compare (Object _) (Field _ _)  = GT
    compare (Object h) (Object k)   = compare h k

instance ContentNode ObjectNode where
    type Id ObjectNode = ObjectNode
    type Data ObjectNode = Text
    
    nodeId = id
    
    nodeName (Field n _) = n
    nodeName (Object k)  = className k

    payload (Field _ v)  = v
    payload (Object k)   = Text.pack ∘ show ∘ entityId $ k


type ObjectTree t = t ObjectNode
instance (Functor t, Tree t) ⇒ ContentTree t ObjectNode

field ∷ Tree t ⇒ Text → Text → ObjectTree t
field name value = mkTree (Field name value) []

object ∷ Tree t ⇒ Text → Integer → ([ObjectTree t] → ObjectTree t)
object className entityId = mkTree (Object entityKey)
    where
    entityKey = fromMaybe btmEntityKey $ mkEntityKey className entityId

addChildren ∷ Tree t ⇒ ObjectTree t → [ObjectTree t] → ObjectTree t
addChildren t cs = changeChildren (++cs) t
