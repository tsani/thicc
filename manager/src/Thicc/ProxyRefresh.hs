{-# LANGUAGE OverloadedStrings #-}

module Thicc.ProxyRefresh where

import Thicc.Types

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Socket ( shutdown, ShutdownCmd(..) )
import qualified Network.Simple.TCP as TCP

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async

-- | Sends the given configuration to the given server and indicates
-- whether the refresh was successful.
-- 'Left' indicates an exceptional circumstance (TCP connection
-- refused, or connection timeout).
-- The 'Bool' indicates the response from the HAProxy supervisor.
refreshProxy :: T.Text -> IPAddress -> IO (Either T.Text Bool)
refreshProxy payload (IPAddress ip) = do
  putStrLn $ "refreshing proxy with config file"
  putStrLn $ T.unpack payload
  a <- async $ TCP.connect (T.unpack ip) "1500" go
  e <- race (delaySec 3) (waitCatch a) -- wait for the worker for 5 seconds
  pure $ case e of
    Left _ -> Left "timeout"
    Right e ->
      case e of
        Left e -> Left (T.pack $ show e)
        Right b -> Right b
  where
    go (sock, addr) = do
      TCP.send sock (T.encodeUtf8 payload)
      shutdown sock ShutdownSend
      maybe False ("ok" ==) <$> TCP.recv sock 1024

    delaySec = threadDelay . (1000000 *)
