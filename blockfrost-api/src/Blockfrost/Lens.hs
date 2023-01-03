-- | Lenses for Blockfrost types

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Blockfrost.Lens
  where

import Blockfrost.Types
import Blockfrost.Util.LensRules
import Control.Lens

makeLensesWith blockfrostFieldRules ''URLVersion

makeFields ''AccountInfo
makeLensesWith blockfrostFieldRules ''AccountReward
makeFields ''AccountHistory
makeFields ''AccountDelegation
makeFields ''AccountRegistration
makeFields ''AccountWithdrawal
makeFields ''AccountMir

makeLensesWith blockfrostFieldRules ''AddressInfo
makeFields ''AddressDetails
makeFields ''AddressUtxo
makeFields ''AddressTransaction

makeFields ''AssetInfo
makeFields ''AssetDetails
makeFields ''AssetOnChainMetadata
makeFields ''AssetMetadata
makeFields ''AssetHistory
makeFields ''AssetTransaction
makeFields ''AssetAddress

makeFields ''Block

makeFields ''EpochInfo
makeFields ''ProtocolParams
makeFields ''StakeDistribution
makeFields ''PoolStakeDistribution

makeFields ''Genesis

makeFields ''TxMeta
makeFields ''TxMetaJSON
makeFields ''TxMetaCBOR

makeFields ''Network
makeFieldsNoPrefix ''NetworkSupply
makeFieldsNoPrefix ''NetworkStake
makeFieldsNoPrefix ''NetworkEraSummary
makeFieldsNoPrefix ''NetworkEraBound
makeFieldsNoPrefix ''NetworkEraParameters

makeFields ''PoolEpoch
makeFields ''PoolInfo
makeFields ''PoolHistory
makeFields ''PoolMetadata
makeFields ''PoolRelay
makeFields ''PoolDelegator
makeFields ''PoolUpdate
makeFields ''PoolRegistrationAction

makeFields ''Transaction
makeFields ''TransactionUtxos
makeFields ''UtxoInput
makeFields ''UtxoOutput
makeFields ''TransactionStake
makeFields ''TransactionDelegation
makeFields ''TransactionWithdrawal
makeFields ''TransactionMir
makeFields ''TransactionPoolUpdate
makeFields ''PoolUpdateMetadata
makeFields ''TransactionPoolRetiring
makeFields ''TransactionMetaJSON
makeFields ''TransactionMetaCBOR
makeFields ''TransactionRedeemer

makeLensesWith blockfrostFieldRules ''IPFSAdd
makeLensesWith blockfrostFieldRules ''IPFSPinChange
makeLensesWith blockfrostFieldRules ''IPFSPin

makeFields ''NutlinkAddress
makeFields ''NutlinkAddressTicker
makeFields ''NutlinkTicker

-- * Shared
makeFields ''BlockIndex
makePrisms ''Amount
