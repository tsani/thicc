module Thicc.Types where

import qualified Data.Text as T
import Data.Word

newtype ServiceId = ServiceId { unServiceId :: T.Text }
  deriving (Eq, Ord, Show)

newtype WorkerId = WorkerId { unWorkerId :: Int }
  deriving (Eq, Ord, Show)

data ServiceConfig = ServiceConfig
  { serviceConfigName :: T.Text -- ^ The name of the service to create.
  , serviceCommand :: String -- ^ The command to run on each worker booted into the service.
  }
  deriving (Eq, Ord, Show)

data ScaleConfig = ScaleConfig
  { scaleConfigServiceId :: ServiceId
  , scaleConfigNumber :: Int -- ^ Absolute number of replicas to have in the service.
  }

data Service = Service
  { serviceProxyIP :: Word32
    -- ^ The IP address of the load balancer for this service.
  , serviceWorkers :: [Worker]
  }
  deriving (Eq, Ord, Show)

newtype IPAddress = IPAddress { ipAddress :: Word32 }
  deriving (Eq, Ord, Show)

data Worker = Worker
  { workerId :: WorkerId
  , workerIP :: IPAddress
    -- ^ The IP address of the worker.
  }
  deriving (Eq, Ord, Show)
