module Thicc.WAL where

import Thicc.Types

data LogEntry
  = CreateService ServiceConfig
  | BootWorker ServiceId
  | KillWorker ServiceId WorkerId
  | ProxyRefresh ServiceId (Maybe [WorkerId])
  | DeleteService ServiceId
  deriving (Eq, Ord, Show)
