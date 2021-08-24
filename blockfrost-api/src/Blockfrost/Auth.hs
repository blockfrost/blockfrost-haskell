-- | Blockfrost authentication schemes
{-# LANGUAGE PolyKinds #-}

module Blockfrost.Auth
  ( APIKeyInHeader
  , APIKeyInHeaderSettings (..)
  , Env (..)
  , Project (..)
  , ProjectAuth
  , mkProject
  , mkProjectEnv
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text as Text
import GHC.Generics (Generic)
import GHC.TypeLits
import Servant.API (HasLink (..), (:>))

import qualified Data.Text

import Data.String (IsString (..))

import Blockfrost.Env

-- * Authentication result and clients token

data Project = Project {
    projectEnv :: Env
  , projectId  :: Text
  }
  deriving (Eq, Show, Generic)

-- | Parse @Project@ from @Text@
mkProject' :: Text -> Either Text Project
mkProject' t =
  let st = Data.Text.strip t
      tEnv = Data.Text.dropEnd 32 st
      token = Data.Text.drop (Data.Text.length tEnv) st
  in Project <$> parseEnv tEnv <*> pure token

-- | Parse @Project@ from @Text@ or fail with error
mkProject :: Text -> Project
mkProject = either (error . Data.Text.unpack) id . mkProject'

-- | @Project@ constructor
mkProjectEnv :: Env -> Text -> Project
mkProjectEnv = Project

instance IsString Project where
  fromString = mkProject . Data.Text.pack

-- * Common

-- | The type of Auth scheme.
data APIKeyInHeader (headerName :: Symbol)

-- | Auth scheme settings
-- Needs IO action to verify passed in token and maybe return Project
newtype APIKeyInHeaderSettings =
  APIKeyInHeaderSettings
    { apiKeySettingsQueryProject :: Text -> IO (Maybe Project)
    }

-- * Custom Auth
-- we use custom ProjectAuth instead of Auth from servant-auth
-- to get rid of superfluous Contexts like JWTSettings and CookiesSettings

data ProjectAuth (auths :: [Type]) val

instance HasLink sub => HasLink (ProjectAuth (tag :: [Type]) value :> sub) where
  type MkLink (ProjectAuth (tag :: [Type]) value :> sub) r = MkLink sub r
  toLink toA _ = toLink toA (Proxy @sub)
