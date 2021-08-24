-- | Internal API error type

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Types.ApiError
  ( ApiError (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | Fancy JSON error returned
-- by the server
data ApiError = ApiError
  { apiError        :: Text
  , apiErrorMessage :: Text
  , apiErrorCode    :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError  where
  toJSON ApiError{..} =
    object [ "error" .= toJSON apiError
           , "message" .= toJSON apiErrorMessage
           , "status_code" .= toJSON apiErrorCode
           ]

instance FromJSON ApiError  where
  parseJSON = withObject "error" $ \o -> do
    apiError <- o .: "error"
    apiErrorMessage <- o .: "message"
    apiErrorCode <- o .: "status_code"
    pure $ ApiError {..}
