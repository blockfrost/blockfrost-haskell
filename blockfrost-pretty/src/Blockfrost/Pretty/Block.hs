-- | Pretty printing Block

{-# LANGUAGE OverloadedStrings #-}

module Blockfrost.Pretty.Block
  ( prettyBlock
  , prettyBlock'
  ) where

import Blockfrost.Lens
import Blockfrost.Pretty.Ada
import Blockfrost.Pretty.Config
import Blockfrost.Pretty.POSIXTime
import Blockfrost.Pretty.Shared
import Blockfrost.Types
import Control.Lens
import Data.Default
import Data.Maybe

import Prettyprinter
import Prettyprinter.Render.Terminal

instance Pretty Block where
  pretty = unAnnotate . prettyBlock

-- | Pretty print `Block` with custom `PrettyConfig`
prettyBlock' :: PrettyConfig -> Block -> Doc AnsiStyle
prettyBlock' cfg b = vcat $
  [ annotate (color Yellow) ("block" <+> prettyBlockHash (b ^. hash))
  , "Date:" <+> prettyTime (b ^. time)
  ]
  ++
  (catMaybes
    [
      ("Height:"<+>) . pretty <$> b ^. height
    , ("Slot:"<+>) . prettySlot <$> b ^. slot
    , ("Epoch:"<+>) . prettyEpoch <$> b ^. epoch
    , ("Slot within the epoch:"<+>) . pretty <$> b ^. epochSlot
    , ("Fees:" <+>) . prettyLovelacesDoc' cfg <$> b ^. fees
    , ("Output:" <+>) . prettyLovelacesDoc' cfg <$> b ^. output
    ]
  )
  ++
  [ "Size:" <+> pretty (b ^. size)
  , "TxCount:" <+> pretty (b ^. txCount)
  , "Confirmations:" <+> pretty (b ^. confirmations)
  , indent 4 $ vsep $ catMaybes
    [ ("VRF:" <+> ) . pretty <$> b ^. blockVrf
    , ("Previous block:" <+> ) . prettyBlockHash <$> b ^. previousBlock
    , ("Next block:" <+> ) . prettyBlockHash <$> b ^. nextBlock
    ]
  ]

-- | Pretty print `Block` using default `PrettyConfig`
prettyBlock :: Block -> Doc AnsiStyle
prettyBlock = prettyBlock' def
