module BaseImport
    ( module BaseImport
    ) where

import Prelude               as BaseImport hiding (head, init, last,
                                                   readFile, tail, writeFile)

import Prelude.Unicode       as BaseImport
import Control.Arrow.Unicode as BaseImport

import Control.Applicative   as BaseImport (pure, (<$>), (<*>))
import Data.Text             as BaseImport (Text)
import Data.Monoid           as BaseImport (Monoid (mappend, mempty, mconcat),
                                            (<>))
