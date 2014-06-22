{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
--
-- CSS to go with an HTML-rendered diff tree.
--
module ExtRep.DiffTreeCss (stylesheet) where

import Prelude.Unicode
import Clay
import Clay.Display
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TextIO

import ExtRep.CssFontIcon



--
-- call from build script to generate css file; e.g.
--
-- $ cd audit-service
-- $ runhaskell -isrc src/ExtRep/DiffTreeCss.hs > diff-tree.css
--
main ∷ IO ()
main = TextIO.putStr ∘ renderWith compact $ stylesheet

stylesheet ∷ Css
stylesheet = do 
           iconFontFace
           diffView   ? display Clay.Display.table
           diffHeader ? headerStyle
           diffTree   ? do
                        treeContainerStyle
                        diffTreeStyle

treeContainerStyle = do
               backgroundColor solarBase3
               color solarBase00
               padding (px 5) (px 50) (px 5) (px 0)
               marginTop (px 0)

headerStyle = do
            putIconBefore username  userIcon
            putIconBefore timestamp clockIcon
            putIconBefore version   dbIcon
            marginBottom (px 0)
            padding (px 5) (px 0) (px 5) (px 20)
            (username <> timestamp <> version) ? marginRight (px 20)
            backgroundColor (rgba 45 99 96 200)
            color (rgba 255 255 255 165)
            headerFont
            backgroundImages [url "noise.png"]  -- TODO externalize

diffTreeStyle = do
              ul ? do "list-style-type" -: "none"  
              diffIcons
              innerNode
              leafNode
              sections
    where
    innerNode = inner ? do
                  display block
                  margin (px 10) (px 0) (px 10) (px 0)
                  padding (px 5) (px 0) (px 5) (px 0)
                  borderTop solid (px 2) solarBase2
                  borderBottom solid (px 1) solarBase2
              >>
              inner ? payload ? before & do
                  content (stringContent "#")
                  monospaceFont

    leafNode = newPayload ? before & do
                  arrowRIcon
                  marginRight (px 10)
                  marginLeft (px 10)
             >>
             leaf ? nodeName ? after & do
                  content (stringContent ":")
                  monospaceFont

    sections = do
                  nodeName ? serifFont
                  payload ? (monospaceFont >> marginLeft (px 15))
                  newPayload ? (monospaceFont >> color solarBlue)
                  new ? payload ? color solarGreen
                  unchanged ? color solarBase2
                  changed ? payload ? color solarBase1

    diffIcons = do
            putIconBefore changed pencilIcon
            putIconBefore new     plusIcon
            putIconBefore deleted minusIcon                               




putIconBefore ∷ Selector → Css → Css
putIconBefore s icon = s ? before & do
                     icon
                     paddingRight (px 5)

pencilIcon = mkFontIcon "\\e600" >> color solarOrange >> fontSize (pct 80)
userIcon   = mkFontIcon "\\e601"
clockIcon  = mkFontIcon "\\e602" >> fontSize (pct 80)
minusIcon  = mkFontIcon "\\e603" >> color solarOrange >> fontSize (pct 80)
plusIcon   = mkFontIcon "\\e604" >> color solarGreen  >> fontSize (pct 80)
arrowRIcon = mkFontIcon "\\e605" >> color solarOrange >> fontSize (pct 90)
dbIcon     = mkFontIcon "\\e606" >> fontSize (pct 80)


headerFont, serifFont, monospaceFont ∷ Css
headerFont = do 
           fontFamily ["Verdana", "Geneva"] [sansSerif]
           fontSize (px 14)
serifFont = do
          fontFamily ["Palatino Linotype", "Book Antiqua", "Palatino"] [serif]
          fontSize (px 16)
          fontStyle italic
monospaceFont = do
              fontFamily ["Lucida Console", "Monaco"] [monospace]
              fontSize (px 14)


solarBase00, solarBase1, solarBase2, solarBase3, solarOrange, solarGreen, solarBlue ∷ Color
solarBase00 = "#657b83"
solarBase1  = "#93a1a1"
solarBase2  = "#eee8d5"
solarBase3  = "#fdf6e3"
solarOrange = "#cb4b16"
solarGreen  = "#859900"
solarBlue   = "#268bd2"


unchanged, changed, new, deleted, payload, newPayload, 
           nodeName, inner, leaf, 
           diffHeader, username, timestamp, version, 
           diffView, diffTree ∷ Selector
unchanged  = ".unchanged"
changed    = ".changed"
new        = ".new"
deleted    = ".deleted"
payload    = ".payload"
newPayload = ".newPayload"
nodeName   = ".nodeName"
inner      = ".inner"  
leaf       = ".leaf"
diffHeader = ".diffHeader"
username   = ".username"
timestamp  = ".timestamp" 
version    = ".version"
diffTree   = ".diffTree"
diffView   = ".diffView"
