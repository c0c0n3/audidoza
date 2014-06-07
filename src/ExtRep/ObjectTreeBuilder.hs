{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module ExtRep.ObjectTreeBuilder
    ( fromXmlTree
    )
where

import Prelude.Unicode
import Control.Arrow
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class
import Text.XML.HXT.DOM.XmlNode

import Diff.ObjectTree




fromXmlTree ∷ (Tree t, XmlNode ξ) ⇒ t ξ → ObjectTree t
fromXmlTree t = build root t 
    where
    root = objectNode t []  -- force a root, even if there's no object id 

build ∷ (Tree t, XmlNode ξ) ⇒ ObjectTree t → t ξ → ObjectTree t
build p t | isObjectTag t = φ (objectNode t [])
          | isFieldTag t  = field (tagName t) (tagText t)
          | otherwise     = φ p
    where
    φ k = k `addChildren` (map (build k) (getChildren t))


objectNode ∷ (Tree t, XmlNode ξ) ⇒ t ξ → ([ObjectTree t] → ObjectTree t)
objectNode = uncurry object ∘ (tagName &&& (fromMaybe 0 ∘ findObjectId))

objectId ∷ (Tree t, XmlNode ξ) ⇒ t ξ → Integer
objectId = undefined

isFieldTag, isObjectTag, isIdTag ∷ (Tree t, XmlNode ξ) ⇒ t ξ → Bool
isFieldTag  = all isText ∘ getChildren
isObjectTag = any isIdTag ∘ getChildren
isIdTag t   = hasName "id" t ∧ isFieldTag t 

hasName ∷ XmlNode ξ ⇒ Text → ξ → Bool
hasName name = fromMaybe False ∘ fmap (≡ n) ∘ getLocalPart
    where
    n = Text.unpack name

tagName, tagText ∷ (Tree t, XmlNode ξ) ⇒ t ξ → Text
tagName = fromMaybeString ∘ getLocalPart
tagText = Text.concat ∘ map (fromMaybeString ∘ getText) ∘ getChildren 
-- TODO: use Text.XML.HXT.DOM.XmlNode.toText (need to account for CDATA, etc?)

fromMaybeString = Text.pack ∘ fromMaybe []

findObjectId ∷ (Tree t, XmlNode ξ) ⇒ t ξ → Maybe Integer
findObjectId = firstIdTagToInt ∘ filter isIdTag ∘ getChildren
   where
   firstIdTagToInt []    = Nothing
   firstIdTagToInt (t:_) = getText t >>= parseInt
   parseInt d = if (all isDigit d) then (Just $ read d) else Nothing

--isContainerTag ∷ (Tree t, XmlNode ξ) ⇒ t ξ → Bool
--isContainerTag = undefined
