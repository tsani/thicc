{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Thicc.Types where

import Data.Aeson
import Data.Coerce ( coerce )
import qualified Data.IntMap as I
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import Data.Word
import GHC.Generics

newtype ServiceId = ServiceId { unServiceId :: T.Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype WorkerId = WorkerId { unWorkerId :: Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data ServiceConfig = ServiceConfig
  { serviceConfigName :: T.Text -- ^ The name of the service to create.
  , serviceCommand :: String -- ^ The command to run on each worker booted into the service.
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ServiceConfig
instance FromJSON ServiceConfig

data ScaleConfig = ScaleConfig
  { scaleConfigServiceId :: ServiceId
  , scaleConfigNumber :: Int -- ^ Absolute number of replicas to have in the service.
  }
  deriving (Eq, Ord, Show)

type WorkerMap = I.IntMap Worker

insertWorker :: WorkerId -> Worker -> WorkerMap -> WorkerMap
insertWorker wId = I.insert (coerce wId)

deleteWorker :: WorkerId -> WorkerMap -> WorkerMap
deleteWorker wId = I.delete (coerce wId)

data Service = Service
  { serviceProxyIP :: IPAddress
    -- ^ The IP address of the load balancer for this service.
  , serviceWorkers :: WorkerMap
  }
  deriving (Eq, Ord, Show, Generic)

emptyService :: IPAddress -> Service
emptyService ip = Service { serviceProxyIP = ip, serviceWorkers = I.empty }

instance ToJSON Service
instance FromJSON Service

newtype IPAddress =
  IPAddress T.Text
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data Worker = Worker
  { workerIP :: IPAddress
    -- ^ The IP address of the worker.
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Worker
instance ToJSON Worker
