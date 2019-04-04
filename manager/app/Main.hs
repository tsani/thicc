{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Web ( webMain )

import Thicc.Monad
import Thicc.Types

import Control.Applicative ( (<|>) )
import Control.Monad
import Data.Maybe ( fromMaybe )
import Docker.Client

main :: IO ()
main = do
  env <- mkThiccEnv
  -- loads the last state from etcd or uses the empty one
  -- if there is no saved state / if the saved state fails to parse
  st <- do
    st <- loadState env
    case st of
      Nothing -> do
        putStrLn "No state to load"
        pure initialThiccState
      Just x -> do
        putStrLn "Loaded state from etcd"
        pure x

  webMain env st

  -- (e, s) <- runThicc env st $ do
  --   -- create service abc
  --   (sId, s) <- createService ServiceConfig
  --     { serviceConfigName = "abc"
  --     , serviceConfigCommand = ["nc", "-lkp", "80"]
  --     }

  --   -- boot three workers in the service
  --   scaleService ScaleConfig
  --     { scaleConfigServiceId = ServiceId "abc"
  --     , scaleConfigNumber = 2
  --     }

  --   -- boot three workers in the service
  --   scaleService ScaleConfig
  --     { scaleConfigServiceId = ServiceId "abc"
  --     , scaleConfigNumber = 1
  --     }

  --   pure ()

  -- case e of
  --   Left e -> print e
  --   Right ips -> putStrLn "success"

  -- pure ()
    
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
