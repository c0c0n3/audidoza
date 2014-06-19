{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ConstraintKinds #-}
module ExtRep.XmlToObjectTree (fromXml) where

import Prelude.Unicode
import Control.Monad
import Data.List
import Data.Maybe
import Data.Tree.Class
import Text.Read
import Text.XML.HXT.DOM.Util
import Text.XML.HXT.DOM.XmlNode

import Diff.ObjectTree
import Util.Hxt




{-
 Transform: XML into ObjectTree.
 Result will be faithful representation of input as long as XML looks like this:
 
 <rootClassName> 

   <id>an integer which identifies the object instance</id>
   <fieldName>field value</fieldName>
   <anotherFieldName>some value</anotherFieldName>
   <fieldNameOfObjectLink>
        <linkedClassName>
            … (recursively)
       </linked23ClassName>
   </fieldNameOfObjectLink>
   …
   <anotherClassName>
     … (recursively)
   </anotherClassName>
   …
   <someSequenceOfObjects>
       <classX>…<classX>
       <classX>…<classX>
       …
   </someSequenceOfObjects>
   
   … (fields and child objects can be in any order)

 </rootClassName>

-}
fromXml ∷ XTree t ξ ⇒ t ξ → ObjectTree t
fromXml root = case build root of
               []  → object "" 0 []           -- no contents
               [x] → x                        -- root has an id or is a field
               xs  → object (name root) 0 xs  -- root has children but no id  

build ∷ XTree t ξ ⇒ t ξ → [ObjectTree t]
build t | isObject x = [object (name t) (objectId x) subTrees]
        | isField x  = [field (name t) (text t)]
        | otherwise  = subTrees
    where
    x = splitChildren t
    subTrees = concatMap build $ props x


data NodeSplit t ξ = NS { oid ∷ Maybe Integer, props ∷ [t ξ], texts ∷ [t ξ] }
 
isObject (NS (Just _) _ _) = True 
isObject _                 = False

isField (NS Nothing [] _) = True
isField _                 = False

objectId = fromMaybe 0 ∘ oid


splitChildren ∷ XTree t ξ ⇒ t ξ → NodeSplit t ξ
splitChildren t = NS { oid = parseObjectId is, props = ps, texts = xs }
    where
    (is, ps, xs) = foldr maybeAdd ([], [], []) (getChildren t)
    maybeAdd t (is, ps, xs) | isText t  = (is, ps, t:xs)
                            | isIdTag t = (t:is, ps, xs)
                            | isElem t  = (is, t:ps, xs)
                            | otherwise = (is, ps, xs)

parseObjectId ∷ XTree t ξ ⇒ [t ξ] → Maybe Integer
parseObjectId = join ∘ find isJust ∘ map (readMaybe ∘ tagText)

isIdTag ∷ XTree t ξ ⇒ t ξ → Bool
isIdTag t = (isElem t) ∧ ((≡"id") ∘ stringToLower ∘ tagName $ t)

