-- | Pretty shared types

{-# LANGUAGE OverloadedStrings #-}

module Blockfrost.Pretty.Shared
  ( prettyBlockHash
  , prettyEpoch
  , prettySlot
  ) where

import Blockfrost.Types
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | Pretty print `BlockHash`
prettyBlockHash :: BlockHash -> Doc AnsiStyle
prettyBlockHash (BlockHash bh) =
  annotate (color Yellow) ("#" <+> pretty bh)

instance Pretty BlockHash where
  pretty = unAnnotate . prettyBlockHash

-- | Pretty print `Slot`
prettySlot :: Slot -> Doc AnsiStyle
prettySlot =
  annotate (color Cyan) . pretty . unSlot

instance Pretty Slot where
  pretty = unAnnotate . prettySlot

-- | Pretty print `Epoch`
prettyEpoch :: Epoch -> Doc AnsiStyle
prettyEpoch =
  annotate (color Green) . pretty . unEpoch

instance Pretty Epoch where
  pretty = unAnnotate . prettyEpoch
