-- | Information about produced blocks

module Blockfrost.Types.Cardano.Blocks
  ( Block (..)
  ) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (explicitParseField)
import qualified Data.Vector
import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)

import Blockfrost.Types.Shared

-- | Information about a block
data Block = Block
  { _blockTime          :: POSIXTime -- ^ Block creation time in UNIX time
  , _blockHeight        :: Maybe Integer -- ^ Block number
  , _blockHash          :: BlockHash -- ^ Hash of the block
  , _blockSlot          :: Maybe Slot -- ^ Slot number
  , _blockEpoch         :: Maybe Epoch -- ^ Epoch number
  , _blockEpochSlot     :: Maybe Integer -- ^ Slot within the epoch
  , _blockSlotLeader    :: Text -- ^ Bech32 ID of the slot leader or specific block description in case there is no slot leader
  , _blockSize          :: Integer -- ^ Block size in Bytes
  , _blockTxCount       :: Integer -- ^ Number of transactions in the block
  , _blockOutput        :: Maybe Lovelaces -- ^ Total output within the block in Lovelaces
  , _blockFees          :: Maybe Lovelaces -- ^ Total fees within the block in Lovelaces
  , _blockBlockVrf      :: Maybe Text -- ^ VRF key of the block
  , _blockOpCert        :: Maybe Text -- ^ The hash of the operational certificate of the block producer
  , _blockOpCertCounter :: Maybe Quantity -- ^ The value of the counter used to produce the operational certificate
  , _blockPreviousBlock :: Maybe BlockHash -- ^ Hash of the previous block
  , _blockNextBlock     :: Maybe BlockHash -- ^ Hash of the next block
  , _blockConfirmations :: Integer -- ^ Number of block confirmations
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_block", CamelToSnake]] Block

instance ToSample Block where
  toSamples _ = singleSample $ Block
    { _blockTime = 1641338934
    , _blockHeight = pure 15243593
    , _blockHash = "4ea1ba291e8eef538635a53e59fddba7810d1679631cc3aed7c8e6c4091a516a"
    , _blockSlot = pure 412162133
    , _blockEpoch = pure 425
    , _blockEpochSlot = pure 12
    , _blockSlotLeader = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2qnikdy"
    , _blockSize = 3
    , _blockTxCount = 1
    , _blockOutput = pure 128314491794
    , _blockFees = pure 592661
    , _blockBlockVrf = pure "vrf_vk1wf2k6lhujezqcfe00l6zetxpnmh9n6mwhpmhm0dvfh3fxgmdnrfqkms8ty"
    , _blockOpCert = pure "da905277534faf75dae41732650568af545134ee08a3c0392dbefc8096ae177c"
    , _blockOpCertCounter = pure 18
    , _blockPreviousBlock = pure "43ebccb3ac72c7cebd0d9b755a4b08412c9f5dcb81b8a0ad1e3c197d29d47b05"
    , _blockNextBlock = pure "8367f026cf4b03e116ff8ee5daf149b55ba5a6ec6dec04803b8dc317721d15fa"
    , _blockConfirmations = 4698
    }

-- instances for getBlockAffectedAddreses response
instance {-# OVERLAPS #-} ToJSON (Address, [TxHash]) where
  toJSON (addr, txs) = object [
        "address" .= toJSON addr
      , "transactions" .= map (\tx -> object [ "tx_hash" .= toJSON tx ]) txs
      ]

instance {-# OVERLAPS #-} FromJSON (Address, [TxHash]) where
  parseJSON = withObject "addrTxs" $ \o -> do
    addr <- o .: "address"
    txs <- explicitParseField
      (withArray "a" $ \a -> mapM (withObject "txHashes" $ \to -> to .: "tx_hash") (Data.Vector.toList a))
      o
      "transactions"
    pure (addr, txs)
