{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Thicc.Types where

import Data.Aeson
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

data Service = Service
  { serviceProxyIP :: IPAddress
    -- ^ The IP address of the load balancer for this service.
  , serviceWorkers :: [Worker]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Service
instance FromJSON Service

newtype IPAddress =
  IPAddress T.Text
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data Worker = Worker
  { workerId :: WorkerId
  , workerIP :: IPAddress
    -- ^ The IP address of the worker.
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Worker
instance ToJSON Worker
