-- | Governance API endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Governance
  where

import Servant.API
import Servant.API.Generic
import Servant.Docs (DocCapture (..), ToCapture (..))

import Blockfrost.Types.Cardano.Governance
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data GovernanceAPI route =
  GovernanceAPI
    {
      _dreps
        :: route
        :- Summary "Delegate Representatives (DReps)"
        :> Description "Return the information about Delegate Representatives (DReps)."
        :> "dreps"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [DRep]
    , _drep
        :: route
        :- Summary "Specific DRep"
        :> Description "DRep information."
        :> "dreps"
        :> Capture "drep_id" DRepId
        :> Get '[JSON] DRepInfo
    , _drepDelegators
        :: route
        :- Summary "DRep delegators"
        :> Description "List of Drep delegators."
        :> "dreps"
        :> Capture "drep_id" DRepId
        :> "delegators"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [DRepDelegator]
    , _drepMetadata
        :: route
        :- Summary "DRep metadata"
        :> Description "DRep metadata information."
        :> "dreps"
        :> Capture "drep_id" DRepId
        :> "metadata"
        :> Get '[JSON] DRepMeta
    , _drepUpdates
        :: route
        :- Summary "DRep updates"
        :> Description "List of certificate updates to the DRep."
        :> "dreps"
        :> Capture "drep_id" DRepId
        :> "updates"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [DRepUpdate]
    , _drepVotes
        :: route
        :- Summary "DRep votes"
        :> Description "History of DReps votes."
        :> "dreps"
        :> Capture "drep_id" DRepId
        :> "votes"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [DRepVote]
    , _proposals
        :: route
        :- Summary "Proposals"
        :> Description "List of proposals."
        :> "proposals"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [Proposal]
    , _proposal
        :: route
        :- Summary "Specific proposal"
        :> Description "Proposal details."
        :> "proposals"
        :> Capture "hash" TxHash
        :> Capture "cert_index" Integer
        :> Get '[JSON] ProposalInfo
    , _paramProposal
        :: route
        :- Summary "Specific parameters proposal"
        :> Description "Parameter proposal details."
        :> "proposals"
        :> Capture "hash" TxHash
        :> Capture "cert_index" Integer
        :> "parameters"
        :> Get '[JSON] ParamProposal
    , _withdrawalProposal
        :: route
        :- Summary "Specific withdrawals proposal"
        :> Description "Withdrawals proposal details."
        :> "proposals"
        :> Capture "hash" TxHash
        :> Capture "cert_index" Integer
        :> "withdrawals"
        :> Get '[JSON] [WithdrawalProposal]
    , _proposalVotes
        :: route
        :- Summary "Proposal votes"
        :> Description "History of proposal votes."
        :> "proposals"
        :> Capture "hash" TxHash
        :> Capture "cert_index" Integer
        :> "votes"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [ProposalVote]
    , _proposalMeta
        :: route
        :- Summary "Specific proposal metadata"
        :> Description "Proposal metadata information."
        :> "proposals"
        :> Capture "hash" TxHash
        :> Capture "cert_index" Integer
        :> "metadata"
        :> Get '[JSON] ProposalMeta
    } deriving (Generic)

instance ToCapture (Capture "cert_index" Integer) where
  toCapture _ = DocCapture "cert_index" "Index of the certificate within the transaction."
