{-# LANGUAGE UnicodeSyntax #-}
module Test where

import Prelude.Unicode
import Data.Text (Text)
import qualified Data.Text as Text

import TreeUtil

import Diff.Content
import Diff.DiffTree
import Diff.ObjectTree




mkF n v = field (Text.pack n) (Text.pack v)
mkF' x = mkF x x
mkO x i = object (Text.pack x) i


t1 = mkO "root" 1
     [ mkF' "r1"
     , mkO "X" 1
       [ mkF' "x1" 
       , mkF' "x2" 
       , mkO "W" 1 []
       ]
     , mkO "Y" 1
       [ mkF' "y1"
       , mkO "Z" 1 
         [ mkF' "z1"
         , mkF' "z2"
         ]
       ]
     ]

t2 = mkO "root" 1
     [ mkF "r1" "* r1"
     , mkO "X" 1
       [ mkF' "x1" 
       , mkF "x2" "* x2"
       , mkF "x3" "+"
       , mkO "W" 1 
         [ mkF "w1" "+"
         ]
       ]
     , mkO "Y" 1
       [ mkF "y2" "+"
       , mkO "Z" 1 
         [ mkF' "z1"
         , mkF' "z2"
         ]
       ]
     , mkO "Y" 2
       [ mkF' "y2"
       ]
     ]

show_t1 = printTreeC t1
show_t2 = printTreeC t2
show_diff_t1_t2 = printTreeC $ diff t1 t2
