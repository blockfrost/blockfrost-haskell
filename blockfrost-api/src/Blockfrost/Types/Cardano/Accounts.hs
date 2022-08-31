-- | Responses for Cardano accounts queries

module Blockfrost.Types.Cardano.Accounts
  ( AccountInfo (..)
  , RewardType (..)
  , AccountReward (..)
  , AccountHistory (..)
  , AccountDelegation (..)
  , AccountRegistration (..)
  , AccountRegistrationAction (..)
  , AccountWithdrawal (..)
  , AccountMir (..)
  , AddressAssociated (..)
  ) where

import Blockfrost.Types.Shared
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

-- | Information about an account, identified by its stake address
data AccountInfo = AccountInfo
  { _accountInfoStakeAddress       :: Address -- ^ Bech32 stake address
  , _accountInfoActive             :: Bool -- ^ Registration state of an account
  , _accountInfoActiveEpoch        :: Maybe Integer -- ^ Epoch of the most recent action - registration or deregistration
  , _accountInfoControlledAmount   :: Lovelaces  -- ^ Balance of the account in Lovelaces
  , _accountInfoRewardsSum         :: Lovelaces -- ^ Sum of all funds rewards for the account in the Lovelaces
  , _accountInfoWithdrawalsSum     :: Lovelaces -- ^ Sum of all the withdrawals for the account in the Lovelaces
  , _accountInfoReservesSum        :: Lovelaces -- ^ Sum of all funds from reserves for the account in the Lovelaces
  , _accountInfoTreasurySum        :: Lovelaces -- ^ Sum of all funds from treasury for the account in the Lovelaces
  , _accountInfoWithdrawableAmount :: Lovelaces -- ^ Sum of available rewards that haven't been withdrawn yet for the account in the Lovelaces
  , _accountInfoPoolId             :: Maybe PoolId -- ^ Bech32 pool ID that owns the account
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountInfo", CamelToSnake]] AccountInfo

instance ToSample AccountInfo where
  toSamples = pure $ singleSample $ AccountInfo
    { _accountInfoStakeAddress = "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
    , _accountInfoActive = True
    , _accountInfoActiveEpoch = pure 412
    , _accountInfoControlledAmount = 619154618165
    , _accountInfoRewardsSum = 319154618165
    , _accountInfoWithdrawalsSum = 12125369253
    , _accountInfoReservesSum = 319154618165
    , _accountInfoTreasurySum = 12000000
    , _accountInfoWithdrawableAmount = 319154618165
    , _accountInfoPoolId = pure "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    }

-- | Reward type
data RewardType = Leader | Member | PoolDepositRefund
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[CamelToKebab]] RewardType

instance ToSample RewardType where
  toSamples = pure $ samples [ Leader, Member, PoolDepositRefund ]

-- | Reward received by an account
data AccountReward = AccountReward
  { _accountRewardEpoch  :: Epoch -- ^ Epoch of the associated reward
  , _accountRewardAmount :: Lovelaces -- ^ Rewards for given epoch in Lovelaces
  , _accountRewardPoolId :: PoolId -- ^ Bech32 pool ID being delegated to
  , _accountRewardType   :: RewardType -- ^ Reward type
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountReward", CamelToSnake]] AccountReward

instance ToSample AccountReward where
  toSamples = pure $ samples
    [ AccountReward
        { _accountRewardEpoch = 214
        , _accountRewardAmount = 1395265
        , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        , _accountRewardType = Member
        }
    , AccountReward
        { _accountRewardEpoch = 215
        , _accountRewardAmount = 58632
        , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        , _accountRewardType = Leader
        }
    , AccountReward
        { _accountRewardEpoch = 216
        , _accountRewardAmount = 0
        , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        , _accountRewardType = Leader
        }
    , AccountReward
        { _accountRewardEpoch = 217
        , _accountRewardAmount = 1395265
        , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        , _accountRewardType = PoolDepositRefund
        }
    ]

-- | History of accounts stake delegation
data AccountHistory = AccountHistory
  { _accountHistoryActiveEpoch :: Integer -- ^ Epoch in which the stake was active
  , _accountHistoryAmount      :: Lovelaces -- ^ Stake amount in Lovelaces
  , _accountHistoryPoolId      :: PoolId -- ^ Bech32 ID of pool being delegated to
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountHistory", CamelToSnake]] AccountHistory

instance ToSample AccountHistory where
  toSamples  = pure $ samples
    [ AccountHistory
        { _accountHistoryActiveEpoch = 260
        , _accountHistoryAmount = 1395265
        , _accountHistoryPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        }
    , AccountHistory
        { _accountHistoryActiveEpoch = 211
        , _accountHistoryAmount = 22695385
        , _accountHistoryPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        }
    ]

-- | Account delegations and associated transaction IDs
data AccountDelegation = AccountDelegation
  { _accountDelegationActiveEpoch :: Epoch -- ^ Epoch in which the delegation becomes active
  , _accountDelegationTxHash      :: TxHash -- ^ Hash of the transaction containing the delegation
  , _accountDelegationAmount      :: Lovelaces -- ^ Rewards for given epoch in Lovelaces
  , _accountDelegationPoolId      :: PoolId -- ^ Bech32 ID of pool being delegated to
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountDelegation", CamelToSnake]] AccountDelegation

instance ToSample AccountDelegation where
  toSamples = pure $ samples
    [ AccountDelegation
        { _accountDelegationActiveEpoch = 210
        , _accountDelegationTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
        , _accountDelegationAmount = 12695385
        , _accountDelegationPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
        }
    , AccountDelegation
        { _accountDelegationActiveEpoch = 242
        , _accountDelegationTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0"
        , _accountDelegationAmount = 12691385
        , _accountDelegationPoolId = "pool1kchver88u3kygsak8wgll7htr8uxn5v35lfrsyy842nkscrzyvj"
        }
    ]

-- | Registration action
data AccountRegistrationAction = Registered | Deregistered
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] AccountRegistrationAction

instance ToSample AccountRegistrationAction where
  toSamples = pure $ samples [ Registered, Deregistered ]

-- | Account (de)registration
data AccountRegistration = AccountRegistration
  { _accountRegistrationAction :: AccountRegistrationAction -- ^ Action in the certificate
  , _accountRegistrationTxHash :: TxHash -- ^ Hash of the transaction containing the (de)registration certificate
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountRegistration", CamelToSnake]] AccountRegistration

instance ToSample AccountRegistration where
  toSamples = pure $ samples
    [ AccountRegistration
        { _accountRegistrationAction = Registered
        , _accountRegistrationTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
        }
    , AccountRegistration
        { _accountRegistrationAction = Deregistered
        , _accountRegistrationTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0"
        }
    ]

-- | Withdrawal from an account
data AccountWithdrawal = AccountWithdrawal
  { _accountWithdrawalAmount :: Lovelaces -- ^ Withdrawal amount in Lovelaces
  , _accountWithdrawalTxHash :: TxHash -- ^ Hash of the transaction containing the withdrawal
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountWithdrawal", CamelToSnake]] AccountWithdrawal

instance ToSample AccountWithdrawal where
  toSamples = pure $ samples
    [ AccountWithdrawal
        { _accountWithdrawalAmount = 454541212442
        , _accountWithdrawalTxHash = "48a9625c841eea0dd2bb6cf551eabe6523b7290c9ce34be74eedef2dd8f7ecc5"
        }
    , AccountWithdrawal
        { _accountWithdrawalAmount = 97846969
        , _accountWithdrawalTxHash = "4230b0cbccf6f449f0847d8ad1d634a7a49df60d8c142bb8cc2dbc8ca03d9e34"
        }
    ]

-- | Account MIR (Move Instantaneous Reward)
data AccountMir = AccountMir
  { _accountMirAmount :: Lovelaces -- ^ MIR amount in Lovelaces
  , _accountMirTxHash :: TxHash -- ^ Hash of the transaction containing the MIR
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_accountMir", CamelToSnake]] AccountMir

instance ToSample AccountMir where
  toSamples = pure $ samples
    [ AccountMir
        { _accountMirAmount = 6202170
        , _accountMirTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
        }
    , AccountMir
        { _accountMirAmount = 1202170
        , _accountMirTxHash = "1dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
        }
    ]

-- | Address associated with an account address
newtype AddressAssociated = AddressAssociated {_addressAssociatedAddress :: Address}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_addressAssociated", CamelToSnake]] AddressAssociated

instance ToSample AddressAssociated where
  toSamples = pure $ samples
    [ AddressAssociated "addr1qx2kd28nq8ac5prwg32hhvudlwggpgfp8utlyqxu6wqgz62f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sy0f4qd"
    , AddressAssociated "addr1q8j55h253zcvl326sk5qdt2n8z7eghzspe0ekxgncr796s2f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sjmd35m"
    ]
