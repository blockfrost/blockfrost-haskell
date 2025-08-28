{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import Blockfrost.Client hiding (NutLinkAPI(..))
import Control.Monad.IO.Class

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

    allMempoolTxs <-
      getMempoolTransactions prj def def

    if null allMempoolTxs
    then return $ (latestBlocks, ers, allMempoolTxs, Nothing)
    else do let lastTxInMempool = TxHash . unTxHashObject $ last allMempoolTxs
            lastMempoolTx <- getMempoolTransaction prj lastTxInMempool

            return (latestBlocks, ers, allMempoolTxs, Just lastMempoolTx) 

  -- variant accepting @Paged@ and @SortOrder@ arguments
  -- getAccountRewards' "gonnaFail" (page 10) desc
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
          case _inputs mempoolTx of
            [] -> print "No mempool transaction inputs found" -- Should be impossible
            (inp:_) -> do
              let address = Address $ _address inp
              mempoolTxByAddress <- runBlockfrost prj $ getMempoolTransactionsByAddress prj address def def
              print "Mempool transactions by address:"
              print mempoolTxByAddress
