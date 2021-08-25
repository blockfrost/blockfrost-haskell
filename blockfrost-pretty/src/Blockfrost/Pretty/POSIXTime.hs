-- | Pretty POSIXTime

{-# LANGUAGE TypeSynonymInstances #-}

module Blockfrost.Pretty.POSIXTime
  where

import Data.Time
import Data.Time.Clock.POSIX
import Prettyprinter
import Prettyprinter.Render.Terminal

instance Pretty POSIXTime where
  pretty = unAnnotate . prettyTime

-- | Pretty print `POSIXTime` as UTC time
prettyTime :: POSIXTime -> Doc AnsiStyle
prettyTime pt =
  let
    ut = posixSecondsToUTCTime pt
    ymd = formatTime defaultTimeLocale "%F" ut
    hms = formatTime defaultTimeLocale "%X" ut
  in
    annotate (color Green) (pretty ymd)
    <+>
    annotate (color Blue) (pretty hms)
    <+>
    pretty "UTC"
