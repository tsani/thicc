{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thicc.Monad where

import Thicc.Types
import Thicc.WAL as WAL

import Control.Monad ( when )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.Map as M
import Data.Maybe ( isJust )
import Data.Monoid ( (<>) )
import qualified Data.Text as T
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
    , MonadIO
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
    -- ^ An unknown error occurred
  | ServiceAlreadyExists ServiceId
    -- ^ A service already exists by that name
  | NoSuchService ServiceId
    -- ^ The requested service does not exist
  | DockerError DockerError
    -- ^ An internal docker command failed
  | SaveFailed
    -- ^ When a write to etcd fails
  | InvariantViolated T.Text
    -- ^ For when internal invariants are violated.

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

-- | Runs a docker computation that may fail inside the 'Thicc' monad,
-- rethrowing any docker error.
runDockerThicc :: DockerT IO (Either DockerError a) -> Thicc a
runDockerThicc m = do
  h <- asks envHttpHandler
  clientOpts <- asks envClientOpts
  handleDockerError $ runDockerT (clientOpts, h) m
  where
    handleDockerError :: IO (Either DockerError a) -> Thicc a
    handleDockerError ioInput = do
      output <- liftIO ioInput
      case output of
        Left e -> throwError $ DockerError e
        Right x -> return x

-- | Persists the Thicc state to etcd.
save :: Thicc ()
save = do
  bs <- encode <$> get
  client <- asks envGrpcClient
  m <- liftIO $ Etcd.put client "thicc-state" (LBS.toStrict bs) Nothing
  case m of
    Nothing -> throwError SaveFailed
    Just x -> pure ()

-- | Computes the name of the container with the proxy for the given
-- service in it.
proxyName :: ServiceId -> T.Text
proxyName (ServiceId name) = "proxy-" <> name

processLogEntry :: WAL.LogEntry -> ServiceMap -> Thicc ServiceMap
processLogEntry entry map = case entry of
  CreateService serviceConfig -> do
    --create container for proxy
    container <- runDockerThicc $ do
        createContainer
    -- image of proxy                NEED TO CHANGE THIS TO THE RIGHT PROXY IMAGE!!!!!!!!!!!!!!!!!!!!!
          (defaultCreateOpts "e5bb0b621a8b") { hostConfig = defaultHostConfig { networkMode = NetworkNamed "thicc-net" }}
          (Just $ proxyName $ ServiceId $ serviceConfigName serviceConfig)
          

    startOpts <- asks envStartOpts
    runDockerThicc $ startContainer startOpts container
    details <- runDockerThicc (inspectContainer container)
    let ip = networkSettingsIpAddress $ networkSettings details
    return $ M.insert  (ServiceId $ serviceConfigName serviceConfig)  Service {serviceProxyIP = IPAddress ip, serviceWorkers = []} map
  BootWorker serviceId workerId -> _
    -- h <- defaultHttpHandler
    -- e <- runDockerT (clientOpts, h) $ do
    --   e <- createContainer
    --     -- image of worker                NEED TO CHANGE THIS TO THE RIGHT worker IMAGE!!!!!!!!!!!!!!!!!!!!!
    --     (defaultCreateOpts "e5bb0b621a8b") { hostConfig = defaultHostConfig { networkMode = NetworkNamed "thicc-net" }}
    --     CreateOpts
    --       { containerConfig = defaultContainerConfig
    --       , hostConfig = defaultHostConfig
    --       , networkingConfig = _
    --       }
    --     (Just $ "worker-" <> serviceConfigName serviceConfig)
    --     case e of
    --       Left e -> pure $ Left e
    --       Right id -> do
    --         startContainer startOpts id
    --  case e of
    --    Left e -> print e
    --    Right x -> print x

  KillWorker serviceId workerId ->  _
  ProxyRefresh serviceId x -> case x of
    Just [workerId] -> _
    Nothing          -> _
  DeleteService serviceId -> _
 -- pattern entry type
 -- deal with each of the patterns

-- | Runs the given 'Thicc' computation, and catches docker errors
-- that may be thrown in it.
catchDockerError :: Thicc a -> Thicc (Maybe a)
catchDockerError m = (Just <$> m) `catchError` handler where
  handler :: ThiccError -> Thicc (Maybe a)
  handler (DockerError e) = pure Nothing
  handler e = throwError e

findContainer :: T.Text -> Thicc (Maybe Container)
findContainer name = do
  containers <-
    filter (any (name ==) . containerNames) <$>
    runDockerThicc (listContainers defaultListOpts)
  case containers of
    [] -> pure Nothing
    [x] -> pure (Just x)
    _ ->
      throwError
      (InvariantViolated $ "multiple containers exist with name " <> name)

-- | Decides whether the postconditions of the 'LogEntry' are already
-- satisfied.
--
-- This is implemented by actually checking whether any required
-- containers exist or not, and will adjust the 'ThiccState' according
-- to the semantics of the 'LogEntry'.
logEntrySatisfied :: LogEntry -> Thicc Bool
logEntrySatisfied entry =
  case entry of
    CreateService conf -> do
      let id = ServiceId $ serviceConfigName conf
      let proxy = proxyName id
      c <- findContainer proxy
      case c of
        Nothing -> pure False
        Just x -> do
          details <- runDockerThicc $ inspectContainer (containerId x)
          let ip = networkSettingsIpAddress $ networkSettings details
          let service =
                Service { serviceProxyIP = IPAddress ip, serviceWorkers = [] }
          modify $ \s ->
            s { thiccServiceMap = M.insert id service $ thiccServiceMap s }
          pure True

processLog :: Thicc ()
processLog = gets thiccWAL >>= go where
  go [] = pure ()
  go (entry : entries) = do
    -- check if the entry's postconditions are already satisfied
    b <- logEntrySatisfied entry
    when (not b) $ do
      map <- gets thiccServiceMap
      map' <- processLogEntry entry map
      modify $ \s -> s { thiccServiceMap = map' }
    -- after executing the entry, shrink the list of entries to process
    modify $ \s -> s { thiccWAL = entries }
    -- and persist the state to fault-tolerant storage
    save

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
