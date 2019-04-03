{-# LANGUAGE OverloadedStrings #-}

module Thicc.ProxyRefresh where

import Thicc.Types

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
  a <- async $ TCP.connect (T.unpack ip) "1500" go
  e <- race (delaySec 5) (waitCatch a) -- wait for the worker for 5 seconds
  pure $ case e of
    Left _ -> Left "timed out"
    Right e ->
      case e of
        Left e -> Left (T.pack $ show e)
        Right b -> Right b
  where
    go (sock, addr) = do
      TCP.send sock (T.encodeUtf8 payload)
      maybe False ("ok" ==) <$> TCP.recv sock 1024

    delaySec = threadDelay . (1000000 *)
