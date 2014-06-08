{-# LANGUAGE UnicodeSyntax #-}
module Util.Hxt where


import Prelude.Unicode

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree.Class
import Text.XML.HXT.DOM.XmlNode


name, text ∷ (Tree t, XmlNode ξ) ⇒ t ξ → Text
name = Text.pack ∘ tagName
text = Text.pack ∘ tagText

tagName, tagText ∷ (Tree t, XmlNode ξ) ⇒ t ξ → String
tagName = fromMaybe [] ∘ getLocalPart
tagText = concatMap (fromMaybe [] ∘ getText) ∘ getChildren
