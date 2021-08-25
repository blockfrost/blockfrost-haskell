{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import Blockfrost.Client

main = do
  -- reads token from BLOCKFROST_TOKEN_PATH
  -- environment variable. It expects token
  -- prefixed with Blockfrost environment name
  -- e.g.: testnet-someTokenHash
  prj <- projectFromEnv
  res <- runBlockfrost prj $ do
    latestBlocks <- getLatestBlock
    (ers :: Either BlockfrostError [AccountReward]) <-
      tryError $ getAccountRewards "gonnaFail"

    -- variant accepting @Paged@ and @SortOrder@ arguments
    -- getAccountRewards' "gonnaFail" (page 10) desc
    pure (latestBlocks, ers)
  print res
