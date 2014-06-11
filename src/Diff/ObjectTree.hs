{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
--
-- ObjectTree = tree representation of input xml 
-- input xml  = xml representation of object tree that was saved in Billdoza
--
module Diff.ObjectTree 
    ( ObjectTree
    , field
    , object
    , addChildren
    )
where

import Prelude.Unicode
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class

import Diff.Content




data ObjectNode = Field  { fieldName ∷ Text, fieldValue ∷ Text }
                | Object { className ∷ Text, entityId ∷ Integer }

instance Show ObjectNode where
    show (Field n v)  = Text.unpack n ++ " : " ++ Text.unpack v
    show (Object c i) = Text.unpack c ++ " # " ++ show i 

instance Eq ObjectNode where
    (Field m _)  == (Field n _)  = m ≡ n
    (Object c i) == (Object d j) = (c, i) ≡ (d, j)
    _            == _            = False

instance Ord ObjectNode where
    compare (Field m _) (Field n _)   = compare m n
    compare (Field _ _) (Object _ _)  = LT
    compare (Object _ _) (Field _ _)  = GT
    compare (Object c i) (Object d j) = compare (c, i) (d, j)

instance ContentNode ObjectNode where
    type Id ObjectNode = ObjectNode
    type Data ObjectNode = Text
    
    nodeId = id
    
    nodeName (Field n _)  = n
    nodeName (Object n _) = n

    payload (Field _ v)  = v
    payload (Object _ i) = Text.pack ∘ show $ i


type ObjectTree t = t ObjectNode
instance (Functor t, Tree t) ⇒ ContentTree t ObjectNode


field ∷ Tree t ⇒ Text → Text → ObjectTree t
field name value = mkTree (Field name value) []

object ∷ Tree t ⇒ Text → Integer → ([ObjectTree t] → ObjectTree t)
object className entityId = mkTree (Object className entityId)

addChildren ∷ Tree t ⇒ ObjectTree t → [ObjectTree t] → ObjectTree t
addChildren t cs = changeChildren (++cs) t
