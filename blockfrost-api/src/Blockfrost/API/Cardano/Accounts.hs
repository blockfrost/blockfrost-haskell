-- | Accounts API endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Accounts
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Accounts
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data AccountsAPI route =
  AccountsAPI
    {
      _account
        :: route
        :- Summary "Specific account address"
        :> Description "Obtain information about a specific stake account."
        :> Capture "stake_address" Address
        :> Get '[JSON] AccountInfo
    , _accountRewards
        :: route
        :- Summary "Specific reward history"
        :> Description "Obtain information about the reward history of a specific account."
        :> Capture "stake_address" Address
        :> "rewards"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AccountReward]
     , _accountHistory
        :: route
        :- Summary "Account history"
        :> Description "Obtain information about the history of a specific account."
        :> Capture "stake_address" Address
        :> "history"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AccountHistory]
     , _accountDelegations
        :: route
        :- Summary "Account delegation history"
        :> Description "Obtain information about the delegation of a specific account."
        :> Capture "stake_address" Address
        :> "delegations"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AccountDelegation]
     , _accountRegistrations
        :: route
        :- Summary "Account registration history"
        :> Description "Obtain information about the registrations and deregistrations of a specific account."
        :> Capture "stake_address" Address
        :> "registrations"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AccountRegistration]
     , _accountWithdrawals
        :: route
        :- Summary "Account withdrawal history"
        :> Description "Obtain information about the withdrawals of a specific account."
        :> Capture "stake_address" Address
        :> "withdrawals"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AccountWithdrawal]
     , _accountMirs
        :: route
        :- Summary "Account MIR history"
        :> Description "Obtain information about the MIRs of a specific account."
        :> Capture "stake_address" Address
        :> "mirs"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AccountMir]
     , _accountAssociatedAddresses
        :: route
        :- Summary "Account associated addresses"
        :> Description "Obtain information about the addresses of a specific account."
        :> Capture "stake_address" Address
        :> "addresses"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AddressAssociated]
     , _accountAssociatedAssets
        :: route
        :- Summary "Assets associated with the account addresses"
        :> Description "Obtain information about assets associated with addresses of a specific account."
        :> Capture "stake_address" Address
        :> "addresses"
        :> "assets"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [Amount]
    } deriving (Generic)
