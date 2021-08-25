-- | Account queries

module Blockfrost.Client.Cardano.Accounts
  ( getAccount
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
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

accountsClient :: Project -> AccountsAPI (AsClientT BlockfrostClient)
accountsClient = fromServant . _accounts . cardanoClient

getAccount_ :: Project -> Address -> BlockfrostClient AccountInfo
getAccount_ = _account . accountsClient

-- | Obtain information about a specific stake account.
getAccount :: Address -> BlockfrostClient AccountInfo
getAccount a = go (`getAccount_` a)

getAccountRewards_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [AccountReward]
getAccountRewards_ = _accountRewards . accountsClient

-- | Obtain information about the history of a specific account.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAccountRewards' :: Address -> Paged -> SortOrder -> BlockfrostClient [AccountReward]
getAccountRewards' a pg s = go (\p -> getAccountRewards_ p a pg s)

-- | Obtain information about the history of a specific account.
getAccountRewards :: Address -> BlockfrostClient [AccountReward]
getAccountRewards a = getAccountRewards' a def def

getAccountHistory_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [AccountHistory]
getAccountHistory_ = _accountHistory . accountsClient

-- | Obtain information about the history of a specific account.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAccountHistory' :: Address -> Paged -> SortOrder -> BlockfrostClient [AccountHistory]
getAccountHistory' a pg s = go (\p -> getAccountHistory_ p a pg s)

-- | Obtain information about the history of a specific account.
getAccountHistory :: Address -> BlockfrostClient [AccountHistory]
getAccountHistory a = getAccountHistory' a def def

getAccountDelegations_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [AccountDelegation]
getAccountDelegations_ = _accountDelegations . accountsClient

-- | Obtain information about the delegation of a specific account.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAccountDelegations' :: Address -> Paged -> SortOrder -> BlockfrostClient [AccountDelegation]
getAccountDelegations' a pg s = go (\p -> getAccountDelegations_ p a pg s)

-- | Obtain information about the delegation of a specific account.
getAccountDelegations :: Address -> BlockfrostClient [AccountDelegation]
getAccountDelegations a = getAccountDelegations' a def def

getAccountRegistrations_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [AccountRegistration]
getAccountRegistrations_ = _accountRegistrations . accountsClient

-- | Obtain information about the registrations and deregistrations of a specific account.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAccountRegistrations' :: Address -> Paged -> SortOrder -> BlockfrostClient [AccountRegistration]
getAccountRegistrations' a pg s = go (\p -> getAccountRegistrations_ p a pg s)

-- | Obtain information about the registrations and deregistrations of a specific account.
getAccountRegistrations :: Address -> BlockfrostClient [AccountRegistration]
getAccountRegistrations a = getAccountRegistrations' a def def

getAccountWithdrawals_ :: Project -> Address -> Paged -> SortOrder ->  BlockfrostClient [AccountWithdrawal]
getAccountWithdrawals_ = _accountWithdrawals . accountsClient

-- | Obtain information about the withdrawals of a specific account.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAccountWithdrawals' :: Address -> Paged -> SortOrder -> BlockfrostClient [AccountWithdrawal]
getAccountWithdrawals' a pg s = go (\p -> getAccountWithdrawals_ p a pg s)

-- | Obtain information about the withdrawals of a specific account.
getAccountWithdrawals :: Address -> BlockfrostClient [AccountWithdrawal]
getAccountWithdrawals a = getAccountWithdrawals' a def def

getAccountMirs_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [AccountMir]
getAccountMirs_ = _accountMirs . accountsClient

-- | Obtain information about the MIRs of a specific account.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAccountMirs' :: Address -> Paged -> SortOrder -> BlockfrostClient [AccountMir]
getAccountMirs' a pg s = go (\p -> getAccountMirs_ p a pg s)

-- | Obtain information about the MIRs of a specific account.
getAccountMirs :: Address -> BlockfrostClient [AccountMir]
getAccountMirs a = getAccountMirs' a def def
