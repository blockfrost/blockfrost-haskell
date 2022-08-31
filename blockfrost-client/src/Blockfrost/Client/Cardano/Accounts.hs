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
  , getAccountAssociatedAddresses
  , getAccountAssociatedAddresses'
  , getAccountAssociatedAssets
  , getAccountAssociatedAssets'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

accountsClient :: MonadBlockfrost m => Project -> AccountsAPI (AsClientT m)
accountsClient = fromServant . _accounts . cardanoClient

getAccount_ :: MonadBlockfrost m => Project -> Address -> m AccountInfo
getAccount_ = _account . accountsClient

-- | Obtain information about a specific stake account.
getAccount :: MonadBlockfrost m => Address -> m AccountInfo
getAccount a = go (`getAccount_` a)

getAccountRewards_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AccountReward]
getAccountRewards_ = _accountRewards . accountsClient

-- | Obtain information about the history of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountRewards' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AccountReward]
getAccountRewards' a pg s = go (\p -> getAccountRewards_ p a pg s)

-- | Obtain information about the history of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountRewards :: MonadBlockfrost m => Address -> m [AccountReward]
getAccountRewards a = getAccountRewards' a def def

getAccountHistory_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AccountHistory]
getAccountHistory_ = _accountHistory . accountsClient

-- | Obtain information about the history of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountHistory' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AccountHistory]
getAccountHistory' a pg s = go (\p -> getAccountHistory_ p a pg s)

-- | Obtain information about the history of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountHistory :: MonadBlockfrost m => Address -> m [AccountHistory]
getAccountHistory a = getAccountHistory' a def def

getAccountDelegations_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AccountDelegation]
getAccountDelegations_ = _accountDelegations . accountsClient

-- | Obtain information about the delegation of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountDelegations' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AccountDelegation]
getAccountDelegations' a pg s = go (\p -> getAccountDelegations_ p a pg s)

-- | Obtain information about the delegation of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountDelegations :: MonadBlockfrost m => Address -> m [AccountDelegation]
getAccountDelegations a = getAccountDelegations' a def def

getAccountRegistrations_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AccountRegistration]
getAccountRegistrations_ = _accountRegistrations . accountsClient

-- | Obtain information about the registrations and deregistrations of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountRegistrations' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AccountRegistration]
getAccountRegistrations' a pg s = go (\p -> getAccountRegistrations_ p a pg s)

-- | Obtain information about the registrations and deregistrations of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountRegistrations :: MonadBlockfrost m => Address -> m [AccountRegistration]
getAccountRegistrations a = getAccountRegistrations' a def def

getAccountWithdrawals_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder ->  m [AccountWithdrawal]
getAccountWithdrawals_ = _accountWithdrawals . accountsClient

-- | Obtain information about the withdrawals of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountWithdrawals' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AccountWithdrawal]
getAccountWithdrawals' a pg s = go (\p -> getAccountWithdrawals_ p a pg s)

-- | Obtain information about the withdrawals of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountWithdrawals :: MonadBlockfrost m => Address -> m [AccountWithdrawal]
getAccountWithdrawals a = getAccountWithdrawals' a def def

getAccountMirs_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AccountMir]
getAccountMirs_ = _accountMirs . accountsClient

-- | Obtain information about the MIRs of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountMirs' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AccountMir]
getAccountMirs' a pg s = go (\p -> getAccountMirs_ p a pg s)

-- | Obtain information about the MIRs of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountMirs :: MonadBlockfrost m => Address -> m [AccountMir]
getAccountMirs a = getAccountMirs' a def def

getAccountAssociatedAddresses_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AddressAssociated]
getAccountAssociatedAddresses_ = _accountAssociatedAddresses . accountsClient

-- | Obtain information about the addresses of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountAssociatedAddresses' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AddressAssociated]
getAccountAssociatedAddresses' a pg s = go (\p -> getAccountAssociatedAddresses_ p a pg s)

-- | Obtain information about the addresses of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountAssociatedAddresses :: MonadBlockfrost m => Address -> m [AddressAssociated]
getAccountAssociatedAddresses a = getAccountAssociatedAddresses' a def def

getAccountAssociatedAssets_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [Amount]
getAccountAssociatedAssets_ = _accountAssociatedAssets . accountsClient

-- | Obtain information about assets associated with addresses of a specific account.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAccountAssociatedAssets' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [Amount]
getAccountAssociatedAssets' a pg s = go (\p -> getAccountAssociatedAssets_ p a pg s)

-- | Obtain information about assets associated with addresses of a specific account.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAccountAssociatedAssets :: MonadBlockfrost m => Address -> m [Amount]
getAccountAssociatedAssets a = getAccountAssociatedAssets' a def def
