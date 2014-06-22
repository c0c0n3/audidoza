{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-} 
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
--
-- simple html page showing the diff of two given content trees.
--
module Html.DiffPage (printHtml) where

import Prelude.Unicode
import Text.Hamlet (hamlet)
import Data.Text (Text)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Diff.DiffTree
import ExtRep.DiffTreeToHtml



render = undefined

template ∷ Html → undef → Html
template renderedDiffTree = [hamlet|
$doctype 5
<html>
    <head>
        <title>Diff Tree Rendered in HTML
        <link rel="stylesheet" type="text/css" href="diff-tree.css">
    <body>
        <div class="diffView">
             <div class="diffHeader">
             <span class="username" title="User who made the change.">James Bondz
             <span class="timestamp" title="When changed.">21 Aug 2009 13:14 57.123
             <span class="version" title="Change version number.">12345
        <div class="diffTree">
             #{renderedDiffTree}
|]

printHtml ∷ RenderableTree t ξ ⇒ t ξ → t ξ → IO ()
printHtml t₁ t₂ = putStrLn $ renderHtml $ template renderedDiffTree render
    where
    renderedDiffTree = toHtml $ diff t₁ t₂
