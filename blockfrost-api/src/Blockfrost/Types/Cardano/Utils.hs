-- | Cardano Utils responses

module Blockfrost.Types.Cardano.Utils
  ( DerivedAddress (..)
  ) where

import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)

-- | Derived Shelley address
data DerivedAddress = DerivedAddress
  { _derivedAddressXpub    :: Text    -- ^ Hexadecimal xpub
  , _derivedAddressRole    :: Integer -- ^ Account role
  , _derivedAddressIndex   :: Integer -- ^ Address index
  , _derivedAddressAddress :: Text    -- ^ Derived address
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_derivedAddress", CamelToSnake]] DerivedAddress

instance ToSample DerivedAddress where
  toSamples = pure $ singleSample
    DerivedAddress
      { _derivedAddressXpub    = "d507c8f866691bd96e131334c355188b1a1d0b2fa0ab11545075aab332d77d9eb19657ad13ee581b56b0f8d744d66ca356b93d42fe176b3de007d53e9c4c4e7a"
      , _derivedAddressRole    = 0
      , _derivedAddressIndex   = 0
      , _derivedAddressAddress = "addr1q90sqnljxky88s0jsnps48jd872p7znzwym0jpzqnax6qs5nfrlkaatu28n0qzmqh7f2cpksxhpc9jefx3wrl0a2wu8q5amen7"
      }
