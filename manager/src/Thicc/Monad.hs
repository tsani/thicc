{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Thicc.Monad where

import Thicc.Types
import qualified Thicc.WAL as WAL

import Control.Monad ( when )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.Map as M
import Data.Maybe ( isJust )
import Docker.Client
import GHC.Generics
import qualified Network.EtcdV3 as Etcd
import Network.GRPC.Client.Helpers ( GrpcClient, setupGrpcClient )

-- | Describes an abstract monad with the high-level capabilities of the manager.
class MonadThicc m where
  -- | Launches a new service.
  -- Initially, the service has no workers associated with it.
  createService
    :: ServiceConfig -- ^ The name of the service.
    -> m Service

  -- | Launches or removes workers in a service to meet the specified
  -- number of replicas. Returns the IP addresses of all workers in the service.
  scaleService
    :: ScaleConfig -- ^ The service the worker belongs to.
    -> m [IPAddress]

  -- | Deletes a service, stopping all its workers and its proxy.
  deleteService
    :: ServiceId -- ^ The service to delete.
    -> m ()

-- | The concrete Thicc monad, which implements 'MonadThicc'.
newtype Thicc a = Thicc
  { unThicc :: ExceptT ThiccError (StateT ThiccState (ReaderT ThiccEnv IO)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ThiccError
    , MonadState ThiccState
    , MonadReader ThiccEnv
    )

-- | The environment used to run the 'Thicc' monad.
data ThiccEnv = ThiccEnv
  { envHttpHandler :: HttpHandler IO
  , envStartOpts :: StartOpts
  , envClientOpts :: DockerClientOpts
  , envGrpcClient :: GrpcClient
  }

data ThiccError
  = UnknownError String
  | ServiceAlreadyExists ServiceId
  | NoSuchService ServiceId

-- | The persistent state of the manager.
data ThiccState = ThiccState
  { thiccServiceMap :: ServiceMap
  -- ^ The services currently being managed.
  , thiccWAL :: [WAL.LogEntry]
  -- ^ The log entries that remain to execute.
  }
  deriving Generic

type ServiceMap = M.Map ServiceId Service

instance ToJSON ThiccState
instance FromJSON ThiccState

serializeState :: ThiccState -> LBS.ByteString
serializeState state = encode state

-- | Persists the Thicc state to etcd.
save :: Thicc ()
save = do
  bs <- serializeState <$> get
  _ -- write bs to etcd

processLogEntry :: WAL.LogEntry -> ServiceMap -> Thicc ServiceMap
processLogEntry entry map = _

processLog :: Thicc ()
processLog = _

-- | Prepends an entry to the log.
addLogEntry :: WAL.LogEntry -> Thicc ()
addLogEntry entry = do
  modify $ \s -> s { thiccWAL = entry : thiccWAL s }
  save

getService :: ServiceId -> Thicc (Maybe Service)
getService id = M.lookup id <$> gets thiccServiceMap

-- | Variant of 'getService' that throws when the service doesn't exist.
getService' :: ServiceId -> Thicc Service
getService' id = do
  m <- getService id
  case m of
    Just x -> pure x
    Nothing -> throwError (NoSuchService id)
  
-- | Gets the next service ID to use.
isServiceIdAvailable :: ServiceId -> Thicc Bool
isServiceIdAvailable id = isJust <$> getService id

instance MonadThicc Thicc where
  createService config = do
    let id = ServiceId $ serviceConfigName config
    do
      b <- isServiceIdAvailable id
      when (not b) $
        throwError (ServiceAlreadyExists id)

    addLogEntry (WAL.CreateService config)
    processLog

    -- this throws NoSuchService if the id can't be found and maybe we
    -- want a better error because this really shouldn't ever happen
    -- and is indicative of a deeper internal problem
    getService' id

  scaleService config = do
    let serviceId = scaleConfigServiceId config
    workerIds <- map (unWorkerId . workerId) . serviceWorkers <$> getService' serviceId
    let workerCount = length workerIds
    let baseId =
          case workerIds of
            [] -> 0
            _  -> maximum workerIds + 1
    let delta = scaleConfigNumber config - workerCount
    if delta < 0 then do
      let delta' = negate delta
      let idsToKill = take delta' workerIds

      addLogEntry (WAL.ProxyRefresh serviceId (Just $ drop delta' (coerce workerIds)))
      forM_ idsToKill $ \idToKill ->
        addLogEntry $ WAL.KillWorker serviceId (WorkerId idToKill)
    else do
      let newIds = take delta $ iterate (1+) baseId
      forM_ newIds $ \newId ->
        addLogEntry $ WAL.BootWorker serviceId (WorkerId newId)
      when (delta /= 0) $ addLogEntry (WAL.ProxyRefresh serviceId Nothing)

    processLog

    -- after the log has finished being processed, we can extract all
    -- the IP addresses from the service's live workers.
    map workerIP . serviceWorkers <$> getService' serviceId

  deleteService serviceId = do
    addLogEntry (WAL.DeleteService serviceId)
    processLog
  
runThicc :: ThiccEnv -> ThiccState -> Thicc a -> IO (Either ThiccError a, ThiccState)
runThicc env initial (Thicc r) =
  runReaderT (runStateT (runExceptT r) initial) env
  
-- | Constructs a default 'ThiccEnv'.
-- This requires IO in order to instantiate an HTTP handler.
mkThiccEnv :: IO ThiccEnv
mkThiccEnv = do
  grpcClient <- setupGrpcClient grpcClientConf
  h <- defaultHttpHandler
  pure ThiccEnv
    { envHttpHandler = h
    , envStartOpts = startOpts
    , envClientOpts = clientOpts
    , envGrpcClient = grpcClient
    }

startOpts = StartOpts { detachKeys = DefaultDetachKey }
clientOpts = DockerClientOpts
  { apiVer = "v1.24"
  , baseUrl = "http://localhost:4243"
  }
grpcClientConf = Etcd.etcdClientConfigSimple "localhost" 2379 False
