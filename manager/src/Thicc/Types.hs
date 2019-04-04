{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Thicc.Types where

import Control.Applicative ( (<|>) )
import Data.Aeson
import Data.Coerce ( coerce )
import qualified Data.IntMap as I
import Data.Maybe ( fromMaybe )
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import Data.Word
import Docker.Client ( ImageID )
import GHC.Generics

type Command = [T.Text]

newtype ServiceId = ServiceId { unServiceId :: T.Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype WorkerId = WorkerId { unWorkerId :: Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data ServiceConfig = ServiceConfig
  { serviceConfigName :: T.Text -- ^ The name of the service to create.
  , serviceConfigCreate :: CreateService
  -- ^ The command or blob to run on each worker booted into the service.
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
  , serviceExe :: ServiceExe
  }
  deriving (Eq, Show, Generic)

instance Ord Service where
  Service { serviceProxyIP = ip1 } <= Service { serviceProxyIP = ip2 } =
    ip1 <= ip2

emptyService :: IPAddress -> ServiceExe -> Service
emptyService ip exe = Service
  { serviceProxyIP = ip
  , serviceWorkers = I.empty
  , serviceExe = exe
  }

instance ToJSON Service
instance FromJSON Service

newtype IPAddress =
  IPAddress {unIPAddress :: T.Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data Worker = Worker
  { workerIP :: IPAddress
    -- ^ The IP address of the worker.
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Worker
instance ToJSON Worker

newtype BlobName = BlobName { unBlobName :: T.Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data CreateService
  = CreateCommand Command
  | CreateBlob BlobName
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CreateService

instance FromJSON CreateService where
  parseJSON (Object o) =
    (CreateCommand <$> o .: "command")
    <|>
    (CreateBlob <$> o .: "blob")

data ServiceExe
  = ExeCommand Command
  | ExeImage ImageID
  deriving (Eq, Show, Generic)

instance ToJSON ServiceExe
instance FromJSON ServiceExe

defaultWorkerImageId :: T.Text
defaultWorkerImageId = "b08f8312d04f"
