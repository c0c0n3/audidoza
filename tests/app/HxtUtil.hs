{-# LANGUAGE UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module HxtUtil where

import Prelude.Unicode
import Control.Arrow.Unicode

--import Text.Heredoc
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.ShowXml as XS




toTree ∷ ArrowXml cat ⇒ String → cat ξ XmlTree
toTree xml = constA xml ⋙ xreadDoc

xfilter :: ArrowXml hom ⇒ hom XmlTree XmlTree → String → hom ξ String
xfilter f xml = xshow (toTree xml ⋙ f ⋙ indentDoc)

-- apply filter f ∷ ArrowXml cat ⇒ cat XmlTree XmlTree
-- to given xml string and pretty-print result to stdout
-- computation happens in IOSLA (IO, state, list arrows)
xrun f xml = (runX $ xfilter f xml) >>=  printL

-- same as xfilter but input string is actually used (i.e. no constA)
xtran :: ArrowXml hom ⇒ hom XmlTree XmlTree → hom String String
xtran f = xshow $ xreadDoc ⋙ f ⋙ indentDoc

-- same as xrun but in LA (list arrow) only
xrun' :: LA XmlTree XmlTree → String → IO ()
xrun' f xml =  printL $ runLA (xtran f) xml


-- apply transform f ∷ ArrowXml hom ⇒ hom XmlTree ξ to given xml string 
-- useful when the results are not an XML document
--   use printL' to print results to stdout, one per line as in:
--       printL' $ run f xml
-- or when there are multiple result trees/docs 
--   use printTrees/printDocs to print results to stdout
run ∷ LA XmlTree ξ → String → [ξ]
run f xml = runLA (xread ⋙ f) xml

printL  = mapM_ putStrLn
printL' = mapM_ (putStrLn ∘ show)

-- add an empty root to the given forest and pretty-print the resulting tree
printTrees ∷ XmlTrees -> IO ()
printTrees = putStrLn ∘ (XN.formatTree show) ∘ (XN.mkRoot [])

-- convert each tree to XML and print it
printDocs ∷ XmlTrees -> IO ()
printDocs = putStrLn ∘ XS.xshow

{-
-- e.g.
xml = [here|
    <x:r xmlns:x="x-uri" xmlns="default">
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <x:u a="a" b="b">
           some <b>text</b>
        </x:u>
    </x:r>
|]


e1 = xrun this xml                      -- prints above xml doc to stdout
e2 = xrun (this //> hasName "t") xml    -- prints t tag and its childrens
e3 = runX $ toTree xml //> hasName "t"  -- tree of XNode's rooted at t
e4 = printDocs $ run this xml           -- same as e1
e5 = printTrees $ run this xml          -- formatted tree of XNode's representing above xml doc
-}

