
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-} 
--
-- prints to stdout or writes to file CSS generated from the Lucius template for
-- an HTML-rendered diff tree.
--
module Html.DiffTreeCss (printCss, writeCss) where

import Prelude.Unicode
import Text.Lucius (CssUrl, luciusFile, renderCss)
import qualified Data.Text.Lazy.IO as TLIO


render = undefined


outFile = "/home/andrea/playground/audit-service/test/Html/diff-tree.css"

template = $(luciusFile "/home/andrea/playground/audit-service/src/ExtRep/DiffTree.lucius")
rendered = renderCss $ template render

printCss ∷ IO ()
printCss = TLIO.putStrLn rendered

writeCss ∷ IO ()
writeCss = TLIO.writeFile outFile rendered
