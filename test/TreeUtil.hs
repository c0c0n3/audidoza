{-# LANGUAGE UnicodeSyntax #-}
module TreeUtil where

import Prelude.Unicode
import Data.Tree




printTree ∷ Show a ⇒ Tree a → IO ()
printTree =  putStrLn ∘ drawTree ∘ fmap show
