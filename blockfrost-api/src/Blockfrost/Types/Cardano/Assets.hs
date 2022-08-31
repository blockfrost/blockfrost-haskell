-- | Responses for Cardano asset queries

module Blockfrost.Types.Cardano.Assets
  ( AssetInfo (..)
  , AssetDetails (..)
  , AssetOnChainMetadata (..)
  , AssetMetadata (..)
  , AssetHistory (..)
  , AssetAction (..)
  , AssetTransaction (..)
  , AssetAddress (..)
  ) where

import Blockfrost.Types.Shared
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

-- | Asset information, result of listing assets
data AssetInfo = AssetInfo
  { _assetInfoAsset    :: Text
  , _assetInfoQuantity :: Quantity
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetInfo", CamelToSnake]] AssetInfo

instance ToSample AssetInfo where
  toSamples = pure $ samples
    [ AssetInfo
        { _assetInfoAsset = "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
        , _assetInfoQuantity = 1
        }
    , AssetInfo
        { _assetInfoAsset = "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e75d"
        , _assetInfoQuantity = 100000
        }
    , AssetInfo
        { _assetInfoAsset = "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
        , _assetInfoQuantity = 18605647
        }
    ]

-- | On-chain metadata stored in the minting transaction under label 721,
-- community discussion around the standard ongoing at https://github.com/cardano-foundation/CIPs/pull/85
data AssetOnChainMetadata = AssetOnChainMetadata
  { _assetOnChainMetadataName  :: Text -- ^ Name of the asset
  , _assetOnChainMetadataImage :: Text -- ^ URI of the associated asset
  -- TODO: in schema has `additionalProperties: true`
  -- so it can carry more arbitrary properties, keep as Value?
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetOnChainMetadata", CamelToSnake]] AssetOnChainMetadata

instance ToSample AssetOnChainMetadata where
  toSamples = pure $ singleSample
    AssetOnChainMetadata
      { _assetOnChainMetadataName = "My NFT token"
      , _assetOnChainMetadataImage = "ipfs://ipfs/QmfKyJ4tuvHowwKQCbCHj4L5T3fSj8cjs7Aau8V7BWv226"
      }

-- | Asset metadata obtained from Cardano token registry
-- https://github.com/cardano-foundation/cardano-token-registry
data AssetMetadata = AssetMetadata
  { _assetMetadataName        :: Text -- ^ Asset name
  , _assetMetadataDescription :: Text -- ^ Asset description
  , _assetMetadataTicker      :: Maybe Text
  , _assetMetadataUrl         :: Maybe Text -- ^ Asset website
  , _assetMetadataLogo        :: Maybe Text -- ^ Base64 encoded logo of the asset
  , _assetMetadataDecimals    :: Maybe Int -- ^ Number of decimal places of the asset unit
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetMetadata", CamelToSnake]] AssetMetadata

instance ToSample AssetMetadata where
  toSamples = pure $ singleSample
    AssetMetadata
      { _assetMetadataName = "nutcoin"
      , _assetMetadataDescription = "The Nut Coin"
      , _assetMetadataTicker = pure "nutc"
      , _assetMetadataUrl = pure "https://www.stakenuts.com/"
      , _assetMetadataLogo =  pure "iVBORw0KGgoAAAANSUhEUgAAADAAAAAoCAYAAAC4h3lxAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAABmJLR0QA/wD/AP+gvaeTAAAAB3RJTUUH5QITCDUPjqwFHwAAB9xJREFUWMPVWXtsU9cZ/8499/r6dZ3E9rUdO7ZDEgglFWO8KaOsJW0pCLRKrN1AqqYVkqoqrYo0ja7bpElru1WairStFKY9WzaE1E1tx+jokKqwtqFNyhKahEJJyJNgJ37E9r1+3HvO/sFR4vhx7SBtfH/F3/l93/f7ne/4PBxEKYU72dj/ZfH772v1TU+HtqbTaX8wOO01GPQpRVH7JEm+vGHDuq6z7/8jUSoHKtaBKkEUFUXdajDy1hUrmrs6zn/wWS7m7pZVjMUirKGUTnzc+e9xLcTrPPVfZzDz06Sc2lyQGEIyAPzT7Xa+dvE/3e+XLaCxoflHsVj8MAAYs74aa/WHoenwvpkZKeFy2Z5NJlOPUkqXZccFwSSrKjlyffjLH+TL6XTUGTGL/6hklD3ldIrj2M5MRmkLBMcvaRLQ1Nj88sxM/HCBfMP+eu/OYGDqe6l0WmpoqJ/88upgrU7HrQNA/cFg6MlkKiLlBtVUO40cx54BgHvLIT/HJLvdeqh/4NKxogKWN7fsCoUi7xTLxLJ4vLq6ak//wKVOrdXtttrTDMPsqJA8AAAwDErdu3VL3alTf5ma9eWCpoKhn5dKpCiqJxicPucQPVu0FHaInn35yHMcKwPAa4SQ3QCwFgDWUko3qSr5vqqSgTypuEg4Mo/zvA74/Y0rZSnZU8akSHV17k2fXfy0txjI5224kEym1s/1EUI7LBbztweHrkzkizn49LP6U6feepFSeggAQK/n04SQZ8bGrxdeQjZrbRvGzLH5hcibRqOhPplMfS1fIY5jz4xPDBdcGggho2h3z9sOLRazdG3wqp9SMgUlzGZ17SSEPsRx7J8CwfGu3PF57WhqqjfN/VxVJUxKUrIdITAXKpDJKFscosdfaFy0u+/K9aXTmXe0kAcAmA5Nng5Hbj6Tj/wCAYFAcN7uEY3GXGazMSHLqVVFapgBoMPna9yqhRAAgCTJMa3YUjZPgNFkSlWYx5eUkx+0tKx83V3rF+cVYJjruWCe133DIXqMmrNrFSDabRcWkywYmG5XFOW6aHcfb9324CoAgMmbo9MIoXkneCajiAihV/c/8eSiBSw4BxyiZxQA6m7H7FBKT2CMn2MY5jFFUX6ZO+5w2j8aHZ7YH40FByrJD5DnHGAY5uTtIA8AgBDaR4F2Yxb3WizCgmtA4ObUPSazodduqz3Suu0hf0U1cjvgdNSJ1dWWveFwdDUAtAiC2Uopdcdi8c9Zlh3GmDGl05mtAKAvo47EcdwThJCjqqpWFxALlNITomg73tff21GRAJez7iVK4WGGYfoJIQduBsbm7UrLm1ueCoUiv65kpiilw1ZbzcFoZOYoIcRTAn6eYZgXJm+Oni+Vd3YJbdyweSch9HlK6SpVVfcyDDq7Yf3m2XPBIXraKyV/a4b9UkLawbLsZgB4rwR8CyGkw13r+5fX27BckwBAEJ47oKpk8+DgUIdod7fV1vqOAMDrlZLPmqKoB+rrvXIgOP6w0WjYy3Ls5RL4bUk52bVm9fqnCk7M3CXU2ND8+MxM7BcIIftiyRYyntcdHh0bmr0wfmXl6p2SJB2KRmP3l4j7zejYUFtRAQAAgslm1Bv4nyGEDpYiIwjmjw0G/RjP866JiclNqqqWfKLq9fyZkdHBBXcnl9O71GDgD8bj0ncRQqZ8sRgzL9yYHH2pqICsOUTPLgA4CXNeZFmzWIS/YhYfjUZmvqPjuceSckrz25pS2h2cmlhbaBwhzr6kfsnL8Xhif55YYFl23Y3Jkdl7EVMoUSA4/q6qqNsBIPd11e52u45FwtG3CSH7yiEPAGC1Vt9dXGBmanDoygFLlbAjtzZCCMyC6VeaOpA1l9N7l1kwtauKaozHE28YTQaQpeR7+TqjxXheR0fHhhgt2CX1S3clEtKC16HL5djYe+niBU0CcmYA2W21/Qih5ZqDcoxlMZ24MaJJAABA87IVJ8Lh6N65Pr1B/+LIyLUfAhRZQvnM6ah7ZDHkAQB0vK6/HHxNTc2ruT5Zkldn/y5LACFk+2LIAwAwCGl6yGSt88KHXbmrBCHkqEgAz+vWLFZALJb4qNwYhFDhCSknkSwnQ4sVgDFeWg7+gQe2r1tAmkGTFQlACHWVg89nhJA9ot3dphV/eeCLp/Pw6K5IQP0S39uLFXCLwDG7zf1cKZxD9LSlUunHc/12u/2t2Vzl/rzu8zb8PZlM7bwdQgDgPK/nX2nddt+53//ht3LW2dS0fF0iLj2vquojuQFmwXRucPBKa8UCmpe1iOFwpAsAfLdJBFBKwVIlXJ2JxqKCxbwyHkvoCkAlv9/71U+7Oq+UJWDZ0hViJBL1cRynbNq0sSeeiPl6ei4NqIqq6TSmlB7X6bjuTEY5pgWfzwxGPZhMpt39/b3vzvWXFGCzulZjjM/DrauDwcAr8bjcgzGjZUuVBMH8k2uDX7wCAFDr8n2LEPI7SqmhTP6SzVbz6MDlz0/nDpT8EmOM22HOvUeWU2wp8iyLgRL6hk7Hrc2SBwC4MTlykmXZRozxn00mbVcphNA5jJmV+chr6oDd5l6jN/A/TqfSuwEAGITGMIsvGo3GTwTB3Dc2NjGSxdZYq4VIOOoNBANnKE0XPXE3brjHOTQ08k2MmVZOxzVJCbkFIQSCYEphzPaFQuGzTpfjb319PZ8UFXin/5OvrHPg/9HueAH/BSUqOuNZm4fyAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIxLTAyLTE5VDA4OjUyOjI1KzAwOjAwCmFGlgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMS0wMi0xOVQwODo1MjoyMyswMDowMBjsyxAAAAAASUVORK5CYII="
      , _assetMetadataDecimals = pure 6
      }

-- | Details of an asset
data AssetDetails = AssetDetails
  { _assetDetailsAsset             :: Text -- ^ Hex-encoded asset full name
  , _assetDetailsPolicyId          :: PolicyId -- ^ Policy ID of the asset
  , _assetDetailsAssetName         :: Maybe Text -- ^ Hex-encoded asset name of the asset
  , _assetDetailsFingerprint       :: Text -- ^ CIP14 based user-facing fingerprint
  , _assetDetailsQuantity          :: Quantity -- ^ Current asset quantity
  , _assetDetailsInitialMintTxHash :: TxHash -- ^ ID of the initial minting transaction
  , _assetDetailsMintOrBurnCount   :: Integer -- ^ Count of mint and burn transactions
  , _assetDetailsOnchainMetadata   :: Maybe AssetOnChainMetadata
  -- ^ On-chain metadata stored in the minting transaction under label 721,
  -- community discussion around the standard ongoing at https://github.com/cardano-foundation/CIPs/pull/85
  , _assetDetailsMetadata          :: Maybe AssetMetadata
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetDetails", CamelToSnake]] AssetDetails

instance ToSample AssetDetails where
  toSamples = pure $ singleSample
    AssetDetails
      { _assetDetailsAsset = "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
      , _assetDetailsPolicyId = "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a7"
      , _assetDetailsAssetName = pure "6e7574636f696e"
      , _assetDetailsFingerprint = "asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w"
      , _assetDetailsQuantity = 12000
      , _assetDetailsInitialMintTxHash = "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
      , _assetDetailsMintOrBurnCount = 1
      , _assetDetailsOnchainMetadata = pure $
          AssetOnChainMetadata
            { _assetOnChainMetadataName = "My NFT token"
            , _assetOnChainMetadataImage = "ipfs://ipfs/QmfKyJ4tuvHowwKQCbCHj4L5T3fSj8cjs7Aau8V7BWv226"
            }
      , _assetDetailsMetadata = pure $
          AssetMetadata
            { _assetMetadataName = "nutcoin"
            , _assetMetadataDescription = "The Nut Coin"
            , _assetMetadataTicker = pure "nutc"
            , _assetMetadataUrl = pure "https://www.stakenuts.com/"
            , _assetMetadataLogo =  pure "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"
            , _assetMetadataDecimals = pure 6
            }
      }

-- | Action of the asset.
-- Created (`Minted`) or destroyed (`Burned`).
data AssetAction = Minted | Burned
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] AssetAction

instance ToSample AssetAction where
  toSamples = pure $ samples [ Minted, Burned ]

-- | History of an asset
data AssetHistory = AssetHistory
  { _assetHistoryTxHash :: TxHash -- ^ Hash of the transaction containing the asset action
  , _assetHistoryAmount :: Quantity -- ^ Asset amount of the specific action
  , _assetHistoryAction :: AssetAction -- ^ Action executed upon the asset policy
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetHistory", CamelToSnake]] AssetHistory

instance ToSample AssetHistory where
  toSamples = pure $ samples
    [ AssetHistory
        { _assetHistoryTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
        , _assetHistoryAmount = 10
        , _assetHistoryAction = Minted
        }
    , AssetHistory
        { _assetHistoryTxHash = "9c190bc1ac88b2ab0c05a82d7de8b71b67a9316377e865748a89d4426c0d3005"
        , _assetHistoryAmount = 5
        , _assetHistoryAction = Burned
        }
    , AssetHistory
        { _assetHistoryTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0"
        , _assetHistoryAmount = 5
        , _assetHistoryAction = Burned
        }
    ]

-- | Transaction of an asset
data AssetTransaction = AssetTransaction
  { _assetTransactionTxHash      :: TxHash -- ^ Hash of the transaction
  , _assetTransactionTxIndex     :: Integer -- ^ Transaction index within the block
  , _assetTransactionBlockHeight :: Integer -- ^ Block height
  , _assetTransactionBlockTime   :: POSIXTime -- ^ Block creation time in UNIX time
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetTransaction", CamelToSnake]] AssetTransaction

instance ToSample AssetTransaction where
  toSamples = pure $ samples
    [ AssetTransaction
        { _assetTransactionTxHash = "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b"
        , _assetTransactionTxIndex = 6
        , _assetTransactionBlockHeight = 69
        , _assetTransactionBlockTime = 1635505891
        }
    , AssetTransaction
        { _assetTransactionTxHash = "52e748c4dec58b687b90b0b40d383b9fe1f24c1a833b7395cdf07dd67859f46f"
        , _assetTransactionTxIndex = 9
        , _assetTransactionBlockHeight = 4547
        , _assetTransactionBlockTime = 1635505987
        }
     , AssetTransaction
        { _assetTransactionTxHash = "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
        , _assetTransactionTxIndex = 0
        , _assetTransactionBlockHeight = 564654
        , _assetTransactionBlockTime = 1834505492
        }
    ]

-- | An address containing specific asset
data AssetAddress = AssetAddress
  { _assetAddressAddress  :: Address -- ^ Address containing the specific asset
  , _assetAddressQuantity :: Quantity -- ^ Asset quantity on the specific address
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_assetAddress", CamelToSnake]] AssetAddress

instance ToSample AssetAddress where
  toSamples = pure $ samples
    [ AssetAddress
        { _assetAddressAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
        , _assetAddressQuantity = 1
        }
    , AssetAddress
        { _assetAddressAddress = "addr1qyhr4exrgavdcn3qhfcc9f939fzsch2re5ry9cwvcdyh4x4re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qdpvhza"
        , _assetAddressQuantity = 100000
        }
     , AssetAddress
        { _assetAddressAddress = "addr1q8zup8m9ue3p98kxlxl9q8rnyan8hw3ul282tsl9s326dfj088lvedv4zckcj24arcpasr0gua4c5gq4zw2rpcpjk2lq8cmd9l"
        , _assetAddressQuantity = 18605647
        }
    ]
