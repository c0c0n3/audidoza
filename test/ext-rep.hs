{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Test where

import Prelude.Unicode
import Control.Arrow.Unicode
import qualified Data.Text as Text
import Text.XML.HXT.Core
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Pretty

import HxtUtil
import TreeUtil
import XmlData
import qualified Html.DiffPage as DiffPage
import qualified Html.DiffJstPage as DiffJstPage

import Diff.DiffTree
import Diff.ObjectTree
import ExtRep.DiffTreeToHtml
import ExtRep.XmlToObjectTree
import Util.Hxt




showX = xrun this
parseX = fromXml ∘ toXTree ∘ Text.pack
showP = printNTree ∘ parseX


e1 = parseX et1
show_et1 = showX et1
show_parse_et1 = showP et1


t1 = parseX xt1
t2 = parseX xt2
diff_t1_t2 = diff t1 t2


show_xt1 = showX xt1
show_parse_xt1 = showP xt1

show_xt2 = showX xt2
show_parse_xt2 = showP xt2

show_diff_t1_t2 = printNTree diff_t1_t2
show_html_diff_t1_t2 = putStr ∘ renderHtml ∘ toHtml $ diff_t1_t2 
show_htmlpage_diff_t1_t2 = DiffPage.printHtml t1 t2
show_jstpage_diff_t1_t2 = DiffJstPage.printHtml diff_t1_t2 
