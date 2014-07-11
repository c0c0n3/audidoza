{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Site.Layout (bareLayout) where

import Import
import Text.Hamlet




bareLayout ∷ Widget → Handler Html
bareLayout widget = do
           pc <- widgetToPageContent widget
           giveUrlRenderer $(hamletFile "templates/site/bare-layout.hamlet")
