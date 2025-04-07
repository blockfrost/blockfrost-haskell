-- | Information about produced blocks

module Blockfrost.Types.Cardano.Blocks
  ( Block (..)
  , TxHashCBOR (..)
  ) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (explicitParseField)
import qualified Data.Vector
import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)

import Blockfrost.Types.Shared
import Blockfrost.Types.Cardano.Transactions (TransactionCBOR(..))

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

-- | Transaction @TxHash@ to its respective @TransactionCBOR@ wrapper.
-- Intentionally not a Map to preserve ordering
newtype TxHashCBOR = TxHashCBOR { getTxHashCBOR :: (TxHash, TransactionCBOR) }
  deriving stock (Show, Eq, Generic)

instance ToSample TxHashCBOR where
  toSamples _ =
      singleSample
    $ TxHashCBOR
        ( TxHash "6f044565223657acdde3c569d6555b2edbd71bc2bfb4df0b1cce0ef805f606d4"
        , TransactionCBOR "84a300d9010281825820787b3a89b1d32806968b867a4a31f1e33054b573821293a1c915559e34810a3602018282583900bd71b1547ab3ec95725100d0c0fb06da5ffae9cf54fb97e0f52fb9cab51adb784c4997143ba56990c0584111137d02898950245f5db5e2631a05f5e10082583900bd71b1547ab3ec95725100d0c0fb06da5ffae9cf54fb97e0f52fb9cab51adb784c4997143ba56990c0584111137d02898950245f5db5e2631b0000001588345487021a0002922da100818258206a7884092084c018eed71f9df5ff61935ab021f578b2d57d8ffb0b0e8ac0ea285840a2045496acf31996336ef187f9f3c2bd24c24995b540541586cbbc73a1550a50c2fdc3d4b99766c2823eb520866b03a38a60a4eb94f24994f441fb2852447c0ef5f6"
        )

instance ToJSON TxHashCBOR where
  toJSON (TxHashCBOR (k, v)) =
    object
      [ "tx_hash" .= toJSON k
      , "cbor" .= toJSON v
      ]

instance FromJSON TxHashCBOR where
  parseJSON =
    withObject "txHashCBOR" $ \to -> do
      th <- to .: "tx_hash"
      cb <- TransactionCBOR <$> to .: "cbor"
      pure $ TxHashCBOR (th, cb)
