{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import Control.Lens ((^.))
import Blockfrost.Client hiding (NutLinkAPI(..))

main :: IO ()
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
    -- getAccountRewards' "acc" (page 10) desc

    allMempoolTxs <-
      allPages $ \p -> getMempoolTransactions' p def

    if null allMempoolTxs
    then
      pure (latestBlocks, ers, allMempoolTxs, Nothing)
    else do
      let lastTxInMempool = TxHash . unTxHashObject $ last allMempoolTxs
      lastMempoolTx <- getMempoolTransaction lastTxInMempool

      pure (latestBlocks, ers, allMempoolTxs, Just lastMempoolTx)

  case res of
    Left e -> print e
    Right ((latestBlocks, ers, allMempoolTxs, lastMempoolTx)) -> do
      print "Latest blocks:"
      print latestBlocks
      putStrLn ""
      print "Account rewards (expected to error):"
      print ers
      putStrLn ""
      print "All mempool transactions (mempool potentially empty):"
      print allMempoolTxs
      putStrLn ""
      print "Last mempool transaction (if any):"
      print lastMempoolTx
      putStrLn ""

      case lastMempoolTx of
        Nothing -> print "No mempool transactions found."
        Just mempoolTx -> do
          case mempoolTx ^. inputs of
            [] -> print "No mempool transaction inputs found" -- Should be impossible
            (inp:_) -> do
              case inp ^. address of
                Nothing -> print "Input has no address"
                Just addr -> do
                  mempoolTxByAddress <-
                    runBlockfrost prj
                      $ getMempoolTransactionsByAddress addr
                  print "Mempool transactions by address:"
                  print mempoolTxByAddress
