-- | Governance queries

module Blockfrost.Client.Cardano.Governance
  ( getDReps
  , getDReps'
  , getDRep
  , getDRepDelegators
  , getDRepDelegators'
  , getDRepMetadata
  , getDRepUpdates
  , getDRepUpdates'
  , getDRepVotes
  , getDRepVotes'
  , getProposals
  , getProposals'
  , getProposal
  , getParamProposal
  , getWithdrawalProposal
  , getProposalVotes
  , getProposalVotes'
  , getProposalMetadata
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

governanceClient :: MonadBlockfrost m => Project -> GovernanceAPI (AsClientT m)
governanceClient = fromServant . _governance . cardanoClient

getDReps_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [DRep]
getDReps_ = _dreps . governanceClient

-- | Return the information about Delegate Representatives (DReps).
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getDReps' :: MonadBlockfrost m => Paged -> SortOrder -> m [DRep]
getDReps' pg s = go (\p -> getDReps_ p pg s)

-- | Return the information about Delegate Representatives (DReps).
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getDReps :: MonadBlockfrost m => m [DRep]
getDReps = getDReps' def def

getDRep_ :: MonadBlockfrost m => Project -> DRepId -> m DRepInfo
getDRep_ = _drep . governanceClient

-- | Return the information about specific Delegate Representative (DRep).
getDRep :: MonadBlockfrost m => DRepId -> m DRepInfo
getDRep d = go (`getDRep_` d)

getDRepDelegators_ :: MonadBlockfrost m => Project -> DRepId -> Paged -> SortOrder -> m [DRepDelegator]
getDRepDelegators_ = _drepDelegators . governanceClient

-- | Get a list of DRep delegators
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getDRepDelegators' :: MonadBlockfrost m => DRepId -> Paged -> SortOrder -> m [DRepDelegator]
getDRepDelegators' d pg s = go (\p -> getDRepDelegators_ p d pg s)

-- | Get a list of DRep delegators
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getDRepDelegators :: MonadBlockfrost m => DRepId -> m [DRepDelegator]
getDRepDelegators d = getDRepDelegators' d def def

getDRepMetadata_ :: MonadBlockfrost m => Project -> DRepId -> m DRepMeta
getDRepMetadata_ = _drepMetadata . governanceClient

-- | Get DRep metadata information.
getDRepMetadata :: MonadBlockfrost m => DRepId -> m DRepMeta
getDRepMetadata d = go (`getDRepMetadata_` d)

getDRepUpdates_ :: MonadBlockfrost m => Project -> DRepId -> Paged -> SortOrder -> m [DRepUpdate]
getDRepUpdates_ = _drepUpdates . governanceClient

-- | Get a list of certificate updates to the DRep.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getDRepUpdates' :: MonadBlockfrost m => DRepId -> Paged -> SortOrder -> m [DRepUpdate]
getDRepUpdates' d pg s = go (\p -> getDRepUpdates_ p d pg s)

-- | Get a list of certificate updates to the DRep.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getDRepUpdates :: MonadBlockfrost m => DRepId -> m [DRepUpdate]
getDRepUpdates d = getDRepUpdates' d def def

getDRepVotes_ :: MonadBlockfrost m => Project -> DRepId -> Paged -> SortOrder -> m [DRepVote]
getDRepVotes_ = _drepVotes . governanceClient

-- | Get a history of DReps votes.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getDRepVotes' :: MonadBlockfrost m => DRepId -> Paged -> SortOrder -> m [DRepVote]
getDRepVotes' d pg s = go (\p -> getDRepVotes_ p d pg s)

-- | Get a history of DReps votes.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getDRepVotes :: MonadBlockfrost m => DRepId -> m [DRepVote]
getDRepVotes d = getDRepVotes' d def def

getProposals_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [Proposal]
getProposals_ = _proposals . governanceClient

-- | Get a history of DReps proposals.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getProposals' :: MonadBlockfrost m => Paged -> SortOrder -> m [Proposal]
getProposals' pg s = go (\p -> getProposals_ p pg s)

-- | Get a history of DReps proposals.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getProposals :: MonadBlockfrost m => m [Proposal]
getProposals = getProposals' def def

getProposal_ :: MonadBlockfrost m => Project -> TxHash -> Integer -> m ProposalInfo
getProposal_ = _proposal . governanceClient

-- | Get a proposal details.
getProposal :: MonadBlockfrost m => TxHash -> Integer -> m ProposalInfo
getProposal txHash certIdx = go (\p -> getProposal_ p txHash certIdx)

getParamProposal_ :: MonadBlockfrost m => Project -> TxHash -> Integer -> m ParamProposal
getParamProposal_ = _paramProposal . governanceClient

-- | Get a parameter proposal details.
getParamProposal :: MonadBlockfrost m => TxHash -> Integer -> m ParamProposal
getParamProposal txHash certIdx = go (\p -> getParamProposal_ p txHash certIdx)

getWithdrawalProposal_ :: MonadBlockfrost m => Project -> TxHash -> Integer -> m [WithdrawalProposal]
getWithdrawalProposal_ = _withdrawalProposal . governanceClient

-- | Get a witdhrawal proposal details.
getWithdrawalProposal :: MonadBlockfrost m => TxHash -> Integer -> m [WithdrawalProposal]
getWithdrawalProposal txHash certIdx = go (\p -> getWithdrawalProposal_ p txHash certIdx)

getProposalVotes_ :: MonadBlockfrost m => Project -> TxHash -> Integer -> Paged -> SortOrder -> m [ProposalVote]
getProposalVotes_ = _proposalVotes . governanceClient

-- | Get a history of DReps proposals.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getProposalVotes' :: MonadBlockfrost m => TxHash -> Integer -> Paged -> SortOrder -> m [ProposalVote]
getProposalVotes' txHash certIdx pg s = go (\p -> getProposalVotes_ p txHash certIdx pg s)

-- | Get a history of DReps proposals.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getProposalVotes :: MonadBlockfrost m => TxHash -> Integer -> m [ProposalVote]
getProposalVotes txHash certIdx = getProposalVotes' txHash certIdx def def

getProposalMetadata_ :: MonadBlockfrost m => Project -> TxHash -> Integer -> m ProposalMeta
getProposalMetadata_ = _proposalMeta . governanceClient

-- | Get a parameter proposal details.
getProposalMetadata :: MonadBlockfrost m => TxHash -> Integer -> m ProposalMeta
getProposalMetadata txHash certIdx = go (\p -> getProposalMetadata_ p txHash certIdx)
