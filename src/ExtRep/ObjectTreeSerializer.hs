{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module ExtRep.ObjectTreeSerializer
    ( parse
    )
where

import Prelude.Unicode
import Control.Arrow.Unicode
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import Text.XML.HXT.Core

import Diff.ObjectTree



{-
 Transform: XML into ObjectTree.
 Result will be faithful representation of input as long as XML looks like this:
 
 <className> 

   <id>an integer which identifies the object instance</id>
   <fieldName>field value</fieldName>
   <anotherFieldName>some value</anotherFieldName>
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

 </className>

-}
parse ∷ (Tree t, ArrowXml hom) ⇒ hom XmlTree (ObjectTree t)
parse = choiceA 
      [ isObjectElement    :-> parseObject
      , isContainerElement :-> (getChildren ⋙ parse)
      , this               :-> parseField
      ]
    where
    isObjectElement    = getObjectId
    isContainerElement = getChildren ⋙ isElem


parseObject ∷ (Tree t, ArrowXml hom) ⇒ hom XmlTree (ObjectTree t)
parseObject = (classAndEntityId ⋙ toObjectNode) &&& (listA $ objectProps ⋙ parse)
            ⋙ (arr $ uncurry ($))
    where  
    classAndEntityId = getName &&& (getObjectId ⋙ deep getText)


type ClassAndEntityId = (String, String)
toObjectNode ∷ (Tree t, ArrowXml hom) ⇒ hom ClassAndEntityId ([ObjectTree t] → ObjectTree t)
toObjectNode = arr $ (uncurry object) ∘ (Text.pack *** parseInt)
    where
    parseInt d = if (all isDigit d) then read d else (-1)


parseField ∷ (Tree t, ArrowXml hom) ⇒ hom XmlTree (ObjectTree t)
parseField = fieldNameAndValue ⋙ toFieldNode
    where
    fieldNameAndValue = getName &&& deep getText


type NameValue = (String, String)
toFieldNode ∷ (Tree t, ArrowXml hom) ⇒ hom NameValue (ObjectTree t)
toFieldNode = arr $ (uncurry field) ∘ (Text.pack *** Text.pack)


objectProps ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
objectProps = getChildren ⋙ isElem ⋙ neg hasObjectId


getObjectId ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
getObjectId = getChildren ⋙ hasObjectId


hasObjectId ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
hasObjectId = hasName "id"

