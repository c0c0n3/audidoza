{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Util.Hxt where

import BaseImport
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Read
import qualified Data.Tree.Class as Tree
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.XmlNode (XmlNode)
import qualified Text.XML.HXT.DOM.XmlNode as XmlNode




type XTree t ξ = (Tree t, XmlNode ξ)


name, text ∷ XTree t ξ ⇒ t ξ → Text
name = Text.pack ∘ tagName
text = Text.pack ∘ tagText


tagName, tagText ∷ XTree t ξ ⇒ t ξ → String
tagName = fromMaybe [] ∘  XmlNode.getLocalPart
tagText = concatMap (fromMaybe [] ∘  XmlNode.getText) ∘ Tree.getChildren


textOf ∷ ArrowXml hom ⇒ String → hom XmlTree Text
textOf tgName = hasName tgName ⋙ multi (getText ⋙ arr Text.pack) >. Text.concat

integerOf ∷ ArrowXml hom ⇒ String → hom XmlTree Integer
integerOf tgName = textOf tgName ⋙ arr parse
    where
    parse = either (const 0) fst ∘ signed decimal ∘ Text.strip

innerXml ∷ ArrowXml hom ⇒ hom XmlTree Text
innerXml = xshow (getChildren ⋙ isElem) ⋙ arr Text.pack

findTag ∷ ArrowXml hom ⇒ String → hom XmlTree XmlTree
findTag tgName = deep $ hasName tgName


toXTree ∷ Text → XmlTree
toXTree xml = case parsed of
              []  → Tree.mkLeaf ∘ XmlNode.mkText $ ""
              t:_ → t
    where
    parsed = runLA (transformDocRoot this) ∘ Text.unpack $ xml 

transformDocRoot ∷ ArrowXml hom ⇒ hom XmlTree XmlTree → hom String XmlTree
transformDocRoot f = xreadDoc ⋙ ((isElem ⋙ f) <+> isError)

transformDocRoot' ∷ ArrowXml hom ⇒ hom XmlTree (Either Text ξ) 
                  → hom String (Either Text ξ)
transformDocRoot' f = xreadDoc ⋙ (transform <+> outputError)
    where
    transform   = isElem ⋙ f
    outputError = isError ⋙ getErrorMsg ⋙ arr (Left ∘ Text.pack)

parseDocRoot ∷ LA XmlTree (Either Text ξ) → Text → Either Text ξ
parseDocRoot f xml = case parsed of
                     []  → Left "no parse"
                     x:_ → x
    where
    parsed = runLA (transformDocRoot' f) ∘ Text.unpack $ xml

toEither ∷ ArrowXml hom ⇒ hom XmlTree ξ → hom XmlTree (Either ε ξ)
toEither f = f ⋙ arr Right 
