{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
--
-- ObjectTree = tree representation of input xml 
-- input xml  = xml representation of object tree that was saved in Billdoza
--
module Diff.ObjectTree 
    ( ObjectTree
    , field
    , object
    , name
    , addChildren
    )
where

import Prelude.Unicode
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs

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
    
    payload (Field _ v)   = v
    payload (Object _ _ ) = Text.empty


type ObjectTree = NTree ObjectNode

field ∷ Text → Text → ObjectTree
field name value = NTree (Field name value) []

object ∷ Text → Integer → ([ObjectTree] → ObjectTree)
object className entityId = NTree (Object className entityId)

name ∷ ObjectTree → Text
name (NTree (Field n _)  _) = n
name (NTree (Object c _) _) = c

addChildren ∷ ObjectTree → [ObjectTree] → ObjectTree
addChildren t cs = changeChildren (++cs) t
