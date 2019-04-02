{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Thicc.WAL where

import Thicc.Types

import Data.Aeson
import GHC.Generics

data LogEntry
  = CreateService ServiceConfig
  | BootWorker ServiceId WorkerId
  | KillWorker ServiceId WorkerId
  | ProxyRefresh ServiceId (Maybe [WorkerId])
  | DeleteService ServiceId
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
