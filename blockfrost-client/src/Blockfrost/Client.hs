-- | Blockfrost client
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blockfrost.Client
  ( module Blockfrost.API
  , module Blockfrost.Env
  , module Blockfrost.Types
  , module Blockfrost.Lens
  , module Blockfrost.Client.Core
  , module Blockfrost.Client.Types
    -- Common
  , getRoot
  , getHealth
  , getClock
    -- Metrics
  , getMetrics
  , getMetricsEndpoints
    -- Cardano - Accounts
  , getAccount
  , getAccountRewards
  , getAccountRewards'
  , getAccountHistory
  , getAccountHistory'
  , getAccountDelegations
  , getAccountDelegations'
  , getAccountRegistrations
  , getAccountRegistrations'
  , getAccountWithdrawals
  , getAccountWithdrawals'
  , getAccountMirs
  , getAccountMirs'
    -- Cardano - Addresses
  , getAddressInfo
  , getAddressDetails
  , getAddressUtxos
  , getAddressUtxos'
  , getAddressTransactions
  , getAddressTransactions'
    -- Cardano - Assets
  , getAssets
  , getAssets'
  , getAssetDetails
  , getAssetHistory
  , getAssetHistory'
  , getAssetTransactions
  , getAssetTransactions'
  , getAssetAddresses
  , getAssetAddresses'
  , getAssetsByPolicy
  , getAssetsByPolicy'
    -- Cardano - Blocks
  , getLatestBlock
  , getLatestBlockTxs
  , getLatestBlockTxs'
  , getBlock
  , getBlockSlot
  , getBlockEpochSlot
  , getNextBlocks
  , getNextBlocks'
  , getPreviousBlocks
  , getPreviousBlocks'
  , getBlockTxs
  , getBlockTxs'
    -- Cardano - Epochs
  , getLatestEpoch
  , getLatestEpochProtocolParams
  , getEpoch
  , getNextEpochs
  , getNextEpochs'
  , getPreviousEpochs
  , getPreviousEpochs'
  , getEpochStake
  , getEpochStake'
  , getEpochStakeByPool
  , getEpochStakeByPool'
  , getEpochBlocks
  , getEpochBlocks'
  , getEpochBlocksByPool
  , getEpochBlocksByPool'
  , getEpochProtocolParams
    -- Cardano - Ledger
  , getLedgerGenesis
    -- Cardano - Metadata
  , getTxMetadataLabels
  , getTxMetadataLabels'
  , getTxMetadataByLabelJSON
  , getTxMetadataByLabelJSON'
  , getTxMetadataByLabelCBOR
  , getTxMetadataByLabelCBOR'
    -- Cardano - Network
  , getNetworkInfo
    -- Cardano - Pools
  , listPools
  , listPools'
  , listRetiredPools
  , listRetiredPools'
  , listRetiringPools
  , listRetiringPools'
  , getPool
  , getPoolHistory
  , getPoolHistory'
  , getPoolMetadata
  , getPoolRelays
  , getPoolDelegators
  , getPoolDelegators'
  , getPoolBlocks
  , getPoolBlocks'
  , getPoolUpdates
  , getPoolUpdates'
    -- Cardano - Transactions
  , getTx
  , getTxUtxos
  , getTxStakes
  , getTxDelegations
  , getTxWithdrawals
  , getTxMirs
  , getTxPoolUpdates
  , getTxPoolRetiring
  , getTxMetadataJSON
  , getTxMetadataCBOR
  , submitTx
    -- IPFS
  , ipfsAdd
  , ipfsGateway
  , ipfsGetPin
  , ipfsListPins
  , ipfsListPins'
  , ipfsPin
  , ipfsRemovePin
    -- Nut.link
  , nutlinkListAddress
  , nutlinkListAddressTickers
  , nutlinkListAddressTickers'
  , nutlinkAddressTickers
  , nutlinkAddressTickers'
  , nutlinkTickers
  , nutlinkTickers'
  ) where

import Blockfrost.API
import Blockfrost.Client.Core
import Blockfrost.Env
import Blockfrost.Lens
import Blockfrost.Types

import Blockfrost.Client.Cardano.Accounts
import Blockfrost.Client.Cardano.Addresses
import Blockfrost.Client.Cardano.Assets
import Blockfrost.Client.Cardano.Blocks
import Blockfrost.Client.Cardano.Epochs
import Blockfrost.Client.Cardano.Ledger
import Blockfrost.Client.Cardano.Metadata
import Blockfrost.Client.Cardano.Network
import Blockfrost.Client.Cardano.Pools
import Blockfrost.Client.Cardano.Transactions
import Blockfrost.Client.IPFS
import Blockfrost.Client.NutLink
import Blockfrost.Client.Types

import Data.Text (Text)

-- ** Client functions
-- *** Health

getRoot' :: Project -> BlockfrostClient URLVersion
getRoot' = _getRoot . commonClient

-- | Root endpoint has no other function than to point end users to documentation
getRoot  :: BlockfrostClient URLVersion
getRoot = go getRoot'

getHealth' :: Project -> BlockfrostClient Healthy
getHealth' = _getHealth . commonClient

-- | Return backend status. Your application should handle situations when backend for the given chain is unavailable.
getHealth  :: BlockfrostClient Healthy
getHealth = go getHealth'

getClock':: Project -> BlockfrostClient ServerTime
getClock' = _getClock . commonClient

-- | Get current backend time
getClock:: BlockfrostClient ServerTime
getClock = go getClock'

getMetrics':: Project -> BlockfrostClient [Metric]
getMetrics' = _metrics . commonClient

-- | Get Blockfrost usage metrics over last 30 days
getMetrics:: BlockfrostClient [Metric]
getMetrics = go getMetrics'

getMetricsEndpoints':: Project -> BlockfrostClient [(Text, Metric)]
getMetricsEndpoints' = _metricsEndpoints . commonClient

-- | Get Blockfrost endpoint usage metrics over last 30 days
getMetricsEndpoints:: BlockfrostClient [(Text, Metric)]
getMetricsEndpoints = go getMetricsEndpoints'
