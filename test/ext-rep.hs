{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Test where

import Prelude.Unicode
import Control.Arrow.Unicode
import Text.XML.HXT.Core
import Text.Blaze.Html.Renderer.Pretty

import HxtUtil
import TreeUtil
import XmlData

import Diff.DiffTree
import Diff.ObjectTree
import ExtRep.DiffTreeToHtml
import ExtRep.XmlToObjectTree




showX = xrun this
parseX = fromXml ∘ head ∘ run (hasName "root")
showP = printNTree ∘ parseX


t1 = parseX xt1
t2 = parseX xt2
diff_t1_t2 = diff t1 t2


show_xt1 = showX xt1
show_parse_xt1 = showP xt1

show_xt2 = showX xt2
show_parse_xt2 = showP xt2

show_diff_t1_t2 = printNTree diff_t1_t2
show_html_diff_t1_t2 = putStr ∘ renderHtml ∘ toHtml5 $ diff_t1_t2 


