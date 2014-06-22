{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-} 
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
--
-- simple html page showing a diff tree with JS Tree controlling the rendered
-- html tree.
--
module Html.DiffJstPage (printHtml) where

import Prelude.Unicode
import Text.Hamlet (HtmlUrl, hamlet)
import Data.Text (Text)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Diff.DiffTree
import ExtRep.DiffTreeToHtml



data MyRoute = JQueryJs | JstJs | JstCss

render :: MyRoute → [(Text, Text)] → Text
render JQueryJs _ = "jstree-dist/libs/jquery.js"
render JstCss _ = "jstree-dist/themes/default/style.min.css"
render JstJs _ = "jstree-dist/jstree.min.js"

jstTagId ∷ Text
jstTagId = "jst-test"

template :: Html → HtmlUrl MyRoute
template renderedDiffTree = [hamlet|
$doctype 5
<html>
    <head>
        <title>Diff Tree Rendered with JS Tree
        
        <script src=@{JQueryJs}>
        <script src=@{JstJs}>
        <link rel=stylesheet href=@{JstCss}>
        
        <script>
            \$(function () { $('##{jstTagId}').jstree(); });

    <body>
        <div id=#{jstTagId}>
            #{renderedDiffTree}
|]

printHtml ∷ RenderableTree t ξ ⇒ DiffTree t ξ → IO ()
printHtml t = putStrLn $ renderHtml $ template (toHtml t) render
