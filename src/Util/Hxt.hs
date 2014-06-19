{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
module Util.Hxt where


import Prelude.Unicode
import Control.Arrow.Unicode

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class (Tree)
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


toXTree ∷ Text → XmlTree
toXTree xml = case parsed of
              []  → Tree.mkLeaf ∘ XmlNode.mkText $ ""
              t:_ → t
    where
    parsed = runLA (transformDocRoot this) ∘ Text.unpack $ xml 

transformDocRoot ∷ ArrowXml hom ⇒ hom XmlTree XmlTree → hom String XmlTree
transformDocRoot f = xreadDoc ⋙ ((isElem ⋙ f) <+> isError)

