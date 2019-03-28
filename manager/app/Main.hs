{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Thicc.Monad
import Thicc.Types

import Control.Monad
import Docker.Client

main :: IO ()
main = do
  env <- mkThiccEnv
  (s, ws) <- runThicc env $ do
    -- create service abc
    s <- createService "abc"
    -- boot three workers in the service
    ws <- forM [1,2,3] $ \_ -> bootWorker (serviceId s)
    pure (s, ws)

  pure ()
    
  -- h <- defaultHttpHandler
  -- e <- runDockerT (clientOpts, h) $ do
  --    e <- createContainer
  --      (defaultCreateOpts "bcdcd7b4a730") { hostConfig = defaultHostConfig { networkMode = NetworkNamed "thicc-net" }}
  --      -- (CreateOpts
  --      --   { containerConfig = defaultContainerConfig
  --      --   , hostConfig = defaultHostConfig
  --      --   , networkingConfig = _
  --      --   })
  --      (Just "test_from_haskell")
  --    case e of
  --      Left e -> pure $ Left e
  --      Right id -> do
  --        startContainer startOpts id
  -- case e of
  --   Left e -> print e
  --   Right x -> print x
