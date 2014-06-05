{-# LANGUAGE UnicodeSyntax #-}
module TreeUtil where

import Prelude.Unicode
import Data.Tree
import qualified Data.Tree.Class as C



printTree ∷ Show a ⇒ Tree a → IO ()
printTree =  putStrLn ∘ drawTree ∘ fmap show

printTreeC ∷ (C.Tree t, Show ξ) ⇒ t ξ → IO ()
printTreeC =  putStrLn ∘ C.formatTree show
