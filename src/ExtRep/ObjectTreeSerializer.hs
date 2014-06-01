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
parse ∷ ArrowXml hom ⇒ hom XmlTree ObjectTree
parse = choiceA 
      [ isObjectElement    :-> parseObject
      , isContainerElement :-> (getChildren ⋙ parse)
      , this               :-> parseField
      ]
    where
    isObjectElement    = getObjectId
    isContainerElement = getChildren ⋙ isElem


parseObject ∷ ArrowXml hom ⇒ hom XmlTree ObjectTree
parseObject = (classAndEntityId ⋙ toObjectNode) &&& (listA $ objectProps ⋙ parse)
            ⋙ (arr $ uncurry ($))
    where  
    classAndEntityId = getName &&& (getObjectId ⋙ deep getText)


type ClassAndEntityId = (String, String)
toObjectNode ∷ ArrowXml hom ⇒ hom ClassAndEntityId ([ObjectTree] → ObjectTree)
toObjectNode = arr $ (uncurry object) ∘ (Text.pack *** parseInt)
    where
    parseInt d = if (all isDigit d) then read d else (-1)


parseField ∷ ArrowXml hom ⇒ hom XmlTree ObjectTree
parseField = fieldNameAndValue ⋙ toFieldNode
    where
    fieldNameAndValue = getName &&& deep getText


type NameValue = (String, String)
toFieldNode ∷ ArrowXml hom ⇒ hom NameValue ObjectTree
toFieldNode = arr $ (uncurry field) ∘ (Text.pack *** Text.pack)


objectProps ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
objectProps = getChildren ⋙ isElem ⋙ neg hasObjectId


getObjectId ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
getObjectId = getChildren ⋙ hasObjectId


hasObjectId ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
hasObjectId = hasName "id"

