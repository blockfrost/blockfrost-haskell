-- | Responses for Cardano address queries

module Blockfrost.Types.Cardano.Addresses
  ( AddressInfo (..)
  , AddressType (..)
  , AddressDetails (..)
  , AddressUTXO (..)
  , AddressTransaction (..)
  ) where

import Blockfrost.Types.Shared
import Deriving.Aeson
import qualified Money
import Servant.Docs (ToSample (..), samples, singleSample)

-- | Information about Cardano address
data AddressInfo = AddressInfo
  { _addressInfoAddress      :: Address -- ^ Bech32 encoded addresses
  , _addressInfoAmount       :: [Amount] -- ^ Lovelaces or tokens stored on this address
  , _addressInfoStakeAddress :: Maybe Address -- ^ Stake address that controls the key
  , _addressInfoType         :: AddressType -- ^ Address era
  , _addressInfoScript       :: Bool -- ^ True if this is a script address
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressInfo", CamelToSnake]] AddressInfo

instance ToSample AddressInfo where
  toSamples = pure $ singleSample
    AddressInfo
      { _addressInfoAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
      , _addressInfoAmount =
        [ AdaAmount 42000000
        , AssetAmount
            $ Money.mkSomeDiscrete
                "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                unitScale
                12
        ]
      , _addressInfoStakeAddress = pure "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
      , _addressInfoType = Shelley
      , _addressInfoScript = False
      }

-- | Type (era) of an address
data AddressType = Byron | Shelley
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] AddressType

instance ToSample AddressType where
  toSamples = pure $ samples [ Byron, Shelley ]

-- | Details about Cardano address
data AddressDetails = AddressDetails
  { _addressDetailsAddress     :: Address -- ^ Bech32 encoded address
  , _addressDetailsReceivedSum :: [Amount] -- ^ Total Lovelaces or tokens received by this address
  , _addressDetailsSentSum     :: [Amount] -- ^ Total Lovelaces or tokens sent by this address
  , _addressDetailsTxCount     :: Integer -- ^ Count of all transactions on the address
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressDetails", CamelToSnake]] AddressDetails

instance ToSample AddressDetails where
  toSamples = pure $ singleSample
    AddressDetails
      { _addressDetailsAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
      , _addressDetailsReceivedSum = amounts
      , _addressDetailsSentSum = amounts
      , _addressDetailsTxCount = 12
      }
    where amounts =
            [ AdaAmount 42000000
            , AssetAmount
                $ Money.mkSomeDiscrete
                        "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                        unitScale
                        12
            ]

-- | UTxOs of the address
data AddressUTXO = AddressUTXO
  { _addressUTXOTxHash      :: TxHash -- ^ Transaction hash of the UTXO
  , _addressUTXOOutputIndex :: Integer -- ^ UTXO index in the transaction
  , _addressUTXOAmount      :: [Amount] -- ^ Amounts of Lovelaces or tokens
  , _addressUTXOBlock       :: BlockHash -- ^ Block hash of the UTXO
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressUTXO", CamelToSnake]] AddressUTXO

instance ToSample AddressUTXO where
  toSamples = pure $ samples
    [ AddressUTXO
      { _addressUTXOTxHash = "39a7a284c2a0948189dc45dec670211cd4d72f7b66c5726c08d9b3df11e44d58"
      , _addressUTXOOutputIndex = 0
      , _addressUTXOAmount = [ AdaAmount 42000000 ]
      , _addressUTXOBlock = "7eb8e27d18686c7db9a18f8bbcfe34e3fed6e047afaa2d969904d15e934847e6"
      }
    , AddressUTXO
      { _addressUTXOTxHash = "4c4e67bafa15e742c13c592b65c8f74c769cd7d9af04c848099672d1ba391b49"
      , _addressUTXOOutputIndex = 0
      , _addressUTXOAmount = [ AdaAmount 729235000 ]
      , _addressUTXOBlock = "953f1b80eb7c11a7ffcd67cbd4fde66e824a451aca5a4065725e5174b81685b7"
      }
    , AddressUTXO
      { _addressUTXOTxHash = "768c63e27a1c816a83dc7b07e78af673b2400de8849ea7e7b734ae1333d100d2"
      , _addressUTXOOutputIndex = 1
      , _addressUTXOAmount =
          [ AdaAmount 42000000
          , AssetAmount
              $ Money.mkSomeDiscrete
                  "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                   unitScale
                   12
          ]
      , _addressUTXOBlock = "5c571f83fe6c784d3fbc223792627ccf0eea96773100f9aedecf8b1eda4544d7"
      }
    ]

-- | Transactions on the address
data AddressTransaction = AddressTransaction {
    _addressTransactionTxHash      :: TxHash -- ^ Hash of the transaction
  , _addressTransactionTxIndex     :: Integer -- ^ Transaction index within the block
  , _addressTransactionBlockHeight :: Integer -- ^ Block height
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressTransaction", CamelToSnake]] AddressTransaction

instance ToSample AddressTransaction where
  toSamples = pure $ samples
    [ AddressTransaction
      { _addressTransactionTxHash = "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b"
      , _addressTransactionTxIndex = 6
      , _addressTransactionBlockHeight = 69
      }
    , AddressTransaction
      { _addressTransactionTxHash = "52e748c4dec58b687b90b0b40d383b9fe1f24c1a833b7395cdf07dd67859f46f"
      , _addressTransactionTxIndex = 9
      , _addressTransactionBlockHeight = 4547
      }
    , AddressTransaction
      { _addressTransactionTxHash = "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
      , _addressTransactionTxIndex = 0
      , _addressTransactionBlockHeight = 564654
      }
    ]
