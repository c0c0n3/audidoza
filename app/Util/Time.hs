{-# LANGUAGE UnicodeSyntax #-}
module Util.Time where

import BaseImport
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Text
import Text.Read
import System.Locale




-- beginning of Unix time: 0 seconds since 1st of January 1970.
epoc ∷ UTCTime
epoc = secondsFromEpoc 0

-- time representing the given number of seconds since the epoc.
secondsFromEpoc ∷ Integer → UTCTime
secondsFromEpoc = posixSecondsToUTCTime ∘ fromIntegral

-- time representing the given number of milliseconds since the epoc.
millisFromEpoc ∷ Integer → UTCTime
millisFromEpoc = posixSecondsToUTCTime ∘ fromRational ∘ (/1000) ∘ fromInteger

-- attempt to read the given string as a number of milliseconds since the epoc;
-- failing that, return the epoc.
readMillisFromEpoc ∷ Text → UTCTime
readMillisFromEpoc = readMillisFromEpocWithDefault epoc

-- attempt to read the given string as a number of milliseconds since the epoc;
-- failing that, return given default.
readMillisFromEpocWithDefault ∷ UTCTime → Text → UTCTime
readMillisFromEpocWithDefault def = fromMaybe def ∘ fmap millisFromEpoc ∘ readMaybe ∘ unpack

-- format time as 'day month year hour:minute second.millisecond'.
-- e.g. 21 Aug 2009 13:14 57.123
showDateTime ∷ UTCTime → Text
showDateTime = pack ∘ formatTime defaultTimeLocale "%d %b %Y %H:%M %S%Q"

