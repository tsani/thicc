{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thicc.Monad
  ( loadState
  , mkThiccEnv
  , runThicc
  , ThiccState
  , initialThiccState
  , ThiccEnv(..)
  , Thicc
  , ThiccError(..)
  , MonadThicc(..)
  )
where

import Thicc.ProxyRefresh
import Thicc.Types
import Thicc.WAL as WAL

import Control.Monad ( when )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.Maybe ( isJust, listToMaybe )
import Data.Monoid ( (<>) )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable ( for )
import Docker.Client
import GHC.Generics
import Lens.Family2 ( (^.) )
import Proto.Etcd.Etcdserver.Etcdserverpb.Rpc_Fields ( kvs, value )
import qualified Network.EtcdV3 as Etcd
import Network.GRPC.Client.Helpers ( GrpcClient, setupGrpcClient )
import System.Directory ( doesFileExist, doesDirectoryExist )
import System.FilePath ( FilePath, (</>) )

base_conf = T.pack "global \nstats timeout 30s \nuser haproxy \ngroup haproxy \ndaemon \n\ndefaults \nmode tcp \ntimeout connect 50s \ntimeout client  50s \ntimeout server  50s \n\nfrontend proxy \nbind *:80 \ndefault_backend service \n\nbackend service \nbalance leastconn"

dockerFileConf = "FROM alpine:3.9\nENTRYPOINT [\"/entry\"]\nCOPY entry /entry\n"
thiccProxyImageId = "12ddb4c54f1f"
thiccStateKey = "thicc-state"

-- | Describes an abstract monad with the high-level capabilities of the manager.
class MonadThicc m where
  -- | Launches a new service.
  -- Initially, the service has no workers associated with it.
  createService
    :: ServiceConfig -- ^ The name of the service.
    -> m (ServiceId, Service)

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
  , envBlobDir :: FilePath
  }

data ThiccError
  = UnknownError String
    -- ^ An unknown error occurred
  | ServiceAlreadyExists ServiceId
    -- ^ A service already exists by that name
  | NoSuchService ServiceId
    -- ^ The requested service does not exist
  | NoSuchContainer T.Text
    -- ^ The requested container does not exist (by name)
  | NoSuchBlob BlobName
    -- ^ When a blob that doesn't exist is requested.
  | DockerError DockerError
    -- ^ An internal docker command failed
  | SaveFailed
    -- ^ When a write to etcd fails
  | InvariantViolated T.Text
    -- ^ For when internal invariants are violated.
  | RefreshFailed T.Text
    -- ^ When a proxy refresh fails.
  | ServiceNonEmpty ServiceId
  deriving Show


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
  m <- liftIO $ Etcd.put client thiccStateKey (LBS.toStrict bs) Nothing
  case m of
    Nothing -> throwError SaveFailed
    Just x -> pure ()

-- | Computes the name of the container with the proxy for the given
-- service in it.
proxyName :: ServiceId -> T.Text
proxyName (ServiceId name) = "proxy-" <> name

-- | Computes the name of the container that hosts a worker.
workerName :: ServiceId -> WorkerId -> T.Text
workerName (ServiceId s) (WorkerId n) = s <> "-worker-" <> T.pack (show n)

-- | Retrives the worker IP address or throws an error if it does not exist
getIP :: ContainerDetails -> Thicc T.Text
getIP details = case (networkSettingsNetworks $ networkSettings details) of
                []   -> throwError (InvariantViolated "No network")
                (Network _ options):xs -> return (networkOptionsIpAddress options)



buildImage :: BlobName -> T.Text -> Thicc ImageID
buildImage blobName serviceId = do
  -- create new docker image
  path <- getBlobPath blobName
  liftIO $ do
    writeFile
      (path </> "Dockerfile")
      dockerFileConf

  out <- runDockerThicc $ buildImageFromDockerfile (defaultBuildOpts serviceId) path

  findImage serviceId


findImage :: T.Text -> Thicc ImageID
findImage imageName = do
  --get image id
  images <- runDockerThicc $ listImages defaultListOpts

  image <- case filter (any (imageName <> ":latest" ==) . imageRepoTags) images of
    [x] -> pure x
    [] -> do
      logMsg $ "finding image " ++ T.unpack imageName
      logMsg $ show images
      throwError $ InvariantViolated "no image after build"
    _ -> do
      throwError (InvariantViolated $ "Multiple images have same tag")

  return $ imageId image




-- | Starts the given container (which must have already been created)
-- and inspects it.
startContainer' :: ContainerID -> Thicc ContainerDetails
startContainer' cId = do
  startOpts <- asks envStartOpts
  runDockerThicc $ startContainer startOpts cId
  runDockerThicc (inspectContainer cId)

logMsg :: String -> Thicc ()
logMsg = liftIO . putStrLn

getBlobPath :: BlobName -> Thicc FilePath
getBlobPath blob@(BlobName name) = do
  base <- asks envBlobDir
  let path = base </> T.unpack name
  b <- liftIO $ doesDirectoryExist path
  when (not b) $ throwError (NoSuchBlob blob)
  pure path

processLogEntry :: WAL.LogEntry -> Thicc ()
processLogEntry entry = case entry of
  CreateService serviceConfig -> do
    --create container for proxy
    let serviceName = serviceConfigName serviceConfig
    let sId = ServiceId serviceName

    exe <- case serviceConfigCreate serviceConfig of
      CreateCommand cmd -> pure $ ExeCommand cmd
      CreateBlob blob -> do
        id <- buildImage blob serviceName
        pure $ ExeImage id

    cId <- runDockerThicc $ do
     -- image of proxy: NEED TO CHANGE THIS TO THE RIGHT PROXY IMAGE!
      let hostConfig = defaultHostConfig { networkMode = NetworkNamed "thicc-net" }

      createContainer
        CreateOpts
        { hostConfig = hostConfig
        , containerConfig =
          (defaultContainerConfig thiccProxyImageId) { cmd = ["dummy"] }
        -- we need to throw in this dummy argument (which gets passed to the supervisor and ignored)
        -- to circumvent a bug in aeson which causes empty lists to be serialized as null
        -- consequently, dockerd fails to parse null back to an empty list.
        -- by putting something in the list, we ensure that dockerd successfully parses the json
        , networkingConfig = NetworkingConfig mempty
        }
        (Just $ proxyName sId)

    logMsg $ "created service proxy " ++ show cId

    details <- startContainer' cId
    logMsg $ "service proxy ready"

    -- actually get the IP address correctly
    ip <- IPAddress <$> getIP details
    let service = emptyService ip exe

    modify $ \s ->
      s { thiccServiceMap = M.insert sId service $ thiccServiceMap s }

  BootWorker serviceId workerId -> do
    logMsg $ "trying to boot " ++ show workerId
    service <- getService' serviceId

    logMsg $ "found " ++ show serviceId

    cId <- runDockerThicc $ do
      let containerName = workerName serviceId workerId
      let hostConfig = defaultHostConfig { networkMode = NetworkNamed "thicc-net" }
      let createOpts = case serviceExe service of
            ExeCommand cmd ->
              let opts = defaultCreateOpts defaultWorkerImageId in
              opts
              { hostConfig = hostConfig
              , containerConfig = (containerConfig opts)
                { cmd = cmd
                }
              }

            ExeImage iId ->
              let opts = defaultCreateOpts (fromImageID iId) in
              opts
              { hostConfig = hostConfig
              , containerConfig = (containerConfig opts)
                { cmd = ["dummy"]
                }
              }

      createContainer createOpts (Just containerName)

    logMsg $ "worker container created"

    details <- startContainer' cId
    ip <- getIP details
    let newWorker = Worker { workerIP = IPAddress ip }

    logMsg $ "started worker " ++ show newWorker

    putService serviceId
      service { serviceWorkers = insertWorker workerId newWorker (serviceWorkers service) }

  KillWorker serviceId wId -> do
    service <- getService' serviceId

    let containerName =
          unServiceId serviceId <> "-worker-" <> T.pack (show(unWorkerId wId))
    --stop worker
    container <- do
      maybeContainer <- findContainer containerName
      case maybeContainer of
        Nothing -> throwError (NoSuchContainer containerName)
        Just x -> pure x

    let cid = containerId container
    runDockerThicc $ stopContainer DefaultTimeout cid
    logMsg $ "stopped " ++ show wId ++ " and waiting"
    runDockerThicc $ waitContainer cid
    --delete container
    runDockerThicc $ deleteContainer defaultContainerDeleteOpts cid
    logMsg $ "deleted " ++ show cid
    --remove from serviceMap

    putService serviceId
      service { serviceWorkers = deleteWorker wId (serviceWorkers service) }

  ProxyRefresh serviceId w -> case w of
    Just w -> do
      --create the conf file
      let refresh_conf = base_conf
      --foreach worker ip in the service
      --add line
      service <- getService' serviceId
      let lines = map (\x -> "\nserver " <> (unIPAddress $ workerIP x) <> " " <> (unIPAddress $ workerIP x) <> ":80") w
      let refreshConf = base_conf <> T.intercalate "" lines

      e <- liftIO $ refreshProxy refreshConf (serviceProxyIP service)
      case e of
        Left e -> throwError (RefreshFailed e)
        Right False -> throwError (RefreshFailed "supervisor did not reply 'ok'")
        Right True -> pure ()

    Nothing -> do
      --look at service and find workers to add
      service <- getService' serviceId
      let workers = I.elems $ serviceWorkers service
      let lines = map (\x -> "\nserver " <> (unIPAddress $ workerIP x) <> " " <> (unIPAddress $ workerIP x) <> ":80") workers
      let refreshConf = base_conf <> T.intercalate "" lines
      --send refresh string
      e <- liftIO $ refreshProxy refreshConf (serviceProxyIP service)
      case e of
        Left e -> throwError (RefreshFailed e)
        Right False -> throwError (RefreshFailed "supervisor did not reply 'ok'")
        Right True -> pure ()

  DeleteService serviceId -> do
    service <- getService' serviceId
    when (not $ I.null $ serviceWorkers service) $ do
      throwError (ServiceNonEmpty serviceId)

    let containerName = "proxy-" <> unServiceId serviceId
    --stop proxy
    container <- do
      maybeContainer <- findContainer containerName
      case maybeContainer of
        Nothing -> throwError (NoSuchContainer containerName)
        Just x -> pure x

    let cid = containerId container
    runDockerThicc $ stopContainer DefaultTimeout cid
    logMsg $ "stopped " ++ show containerName ++ " and waiting"
    runDockerThicc $ waitContainer cid
    --delete container
    runDockerThicc $ deleteContainer defaultContainerDeleteOpts cid
    logMsg $ "deleted " ++ show cid
    --remove from serviceMap
    modify $ \s -> s {thiccServiceMap = M.delete serviceId (thiccServiceMap s)}


-- | Runs the given 'Thicc' computation, and catches docker errors
-- that may be thrown in it.
catchDockerError :: Thicc a -> Thicc (Maybe a)
catchDockerError m = (Just <$> m) `catchError` handler where
  handler :: ThiccError -> Thicc (Maybe a)
  handler (DockerError e) = pure Nothing
  handler e = throwError e

findContainer :: T.Text -> Thicc (Maybe Container)
findContainer name = do
  containers <- do
    -- need to use -a for listing to find container that may not have
    -- been started, then people who call us need to deal with the
    -- fact that it may not be started
    cs <- runDockerThicc (listContainers defaultListOpts)
    let cs' = filter (any ("/" <> name ==) . containerNames) cs
    pure cs'
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
      let serviceName = serviceConfigName conf
      let id = ServiceId serviceName
      let containerName = proxyName id
      m <- getIdIp containerName
      case m of
        Nothing -> pure False
        Just (_, ip) -> do
          exe <- case serviceConfigCreate conf of
            CreateCommand cmd -> pure $ ExeCommand cmd
            CreateBlob _ -> ExeImage <$> findImage serviceName
          modify $ \s ->
            s { thiccServiceMap = M.insert id (emptyService ip exe) $ thiccServiceMap s }
          pure True

    BootWorker sId wId -> do
      service <- getService' sId
      let containerName = workerName sId wId
      m <- getIdIp containerName
      case m of
        Nothing -> pure False
        Just (_, ip) -> do
          let worker = Worker { workerIP = ip }
          let workerMap = insertWorker wId worker (serviceWorkers service)
          putService sId
            service { serviceWorkers = workerMap }
          pure True

    KillWorker sId wId -> do
      service <- getService' sId
      let containerName = workerName sId wId
      -- check if the container is alive
      m <- getIdIp containerName
      case m of
        -- if not, then make sure it's not i nthe worker map for the service
        Nothing -> do
          putService sId
            service { serviceWorkers = deleteWorker wId (serviceWorkers service) }
          logMsg $ "KillWorker postcondition satisfied"
          pure True
        -- if so, then the postconditions are not satisfied
        Just _ -> do
          logMsg $ "KillWorker postcondition not satisfied"
          pure False

    ProxyRefresh _ _ ->
      -- Refresh is idempotent, so there's no harm.
      -- Plus, detecting whether the proxy is running a specific
      -- config file is nontrivial.
      pure False

    DeleteService sId -> do
      m <- getIdIp (proxyName sId)
      case m of
        Nothing -> do
          modify $ \s ->
            s { thiccServiceMap = M.delete sId (thiccServiceMap s) }
          pure True
        Just _ -> pure False

  where
    -- get the IP address of the container with the given name
    getIdIp name = do
      c <- findContainer name
      logMsg $ "found container for name " ++ T.unpack name
      for c $ \x -> do
        let cId = containerId x
        details <- runDockerThicc $ inspectContainer cId
        pure $
          ( cId
          , IPAddress $ networkSettingsIpAddress $ networkSettings details
          )

catchAllTheErrors :: Thicc a -> Thicc (Either ThiccError a)
catchAllTheErrors m = (Right <$> m) `catchError` handler where
  handler e = pure $ Left e

processLog :: Thicc ()
processLog = do
  wal <- gets thiccWAL
  logMsg $ "processing WAL with " ++ show (length wal) ++ " entries"
  go wal
  where
    go [] = pure ()
    go (entry : entries) = do
      -- check if the entry's postconditions are already satisfied
      b <- logEntrySatisfied entry
      e <- catchAllTheErrors $ when (not b) $ processLogEntry entry

      case e of
        Left e -> do
          modify $ \s -> s { thiccWAL = [] }
          save
          throwError e
        Right () -> do
          -- after executing the entry, shrink the list of entries to process
          modify $ \s -> s { thiccWAL = entries }
          -- and persist the state to fault-tolerant storage
          save

      go entries

-- | Prepends an entry to the log.
addLogEntry :: WAL.LogEntry -> Thicc ()
addLogEntry entry = do
  modify $ \s -> s { thiccWAL = thiccWAL s ++ [entry] }
  save

putService :: ServiceId -> Service -> Thicc ()
putService sId service =
  modify $ \s ->
  s { thiccServiceMap = M.insert sId service (thiccServiceMap s) }

getService :: ServiceId -> Thicc (Maybe Service)
getService id = M.lookup id <$> gets thiccServiceMap

-- | Variant of 'getService' that throws when the service doesn't exist.
getService' :: ServiceId -> Thicc Service
getService' id = do
  m <- getService id
  case m of
    Just x -> pure x
    Nothing -> throwError (NoSuchService id)

instance MonadThicc Thicc where
  createService config = do
    let id = ServiceId $ serviceConfigName config
    do
      s <- getService id
      when (isJust s) $ throwError (ServiceAlreadyExists id)

    addLogEntry (WAL.CreateService config)
    processLog

    -- this throws NoSuchService if the id can't be found and maybe we
    -- want a better error because this really shouldn't ever happen
    -- and is indicative of a deeper internal problem
    s <- getService' id
    pure (id, s)

  scaleService config = do
    let serviceId = scaleConfigServiceId config
    workerIds <- I.assocs . serviceWorkers <$> getService' serviceId
    let workerCount = length workerIds
    let baseId =
          case workerIds of
            [] -> 0
            _  -> maximum (map fst workerIds) + 1
    let delta = scaleConfigNumber config - workerCount
    if delta < 0 then do
      let delta' = negate delta
      let idsToKill = take delta' workerIds

      addLogEntry (WAL.ProxyRefresh serviceId (Just $ map snd $ drop delta' (workerIds)))
      forM_ idsToKill $ \(idToKill,_) ->
        addLogEntry $ WAL.KillWorker serviceId (WorkerId idToKill)
    else do
      let newIds = take delta $ iterate (1+) baseId
      forM_ newIds $ \newId ->
        addLogEntry $ WAL.BootWorker serviceId (WorkerId newId)
      when (delta /= 0) $ addLogEntry (WAL.ProxyRefresh serviceId Nothing)

    processLog

    -- after the log has finished being processed, we can extract all
    -- the IP addresses from the service's live workers.
    map workerIP . I.elems . serviceWorkers <$> getService' serviceId

  deleteService serviceId = do
    addLogEntry (WAL.DeleteService serviceId)
    processLog

-- | Runs the given 'Thicc' computation.
--
-- Before the computation is run, any pending operations in the WAL
-- are executed.
runThicc :: ThiccEnv -> ThiccState -> Thicc a -> IO (Either ThiccError a, ThiccState)
runThicc env initial m = runReaderT (runStateT (runExceptT r) initial) env where
  Thicc r = processLog >> m

-- | Constructs a default 'ThiccEnv'.
-- This requires IO in order to instantiate an HTTP handler and to
-- setup the GRPC client.
mkThiccEnv :: IO ThiccEnv
mkThiccEnv = do
  grpcClient <- setupGrpcClient grpcClientConf
  h <- defaultHttpHandler
  pure ThiccEnv
    { envHttpHandler = h
    , envStartOpts = startOpts
    , envClientOpts = clientOpts
    , envGrpcClient = grpcClient
    , envBlobDir = "/home/tsani/projects/thicc/manager/blobs"
    }

-- | Attempts to load the 'ThiccState' from etcd.
loadState :: ThiccEnv -> IO (Maybe ThiccState)
loadState env =
  (decode . LBS.fromStrict . (^. value) =<<) . (listToMaybe . (^. kvs) =<<)
  <$> Etcd.range (envGrpcClient env) (Etcd.SingleKey thiccStateKey)

initialThiccState :: ThiccState
initialThiccState =
  ThiccState { thiccServiceMap = M.empty, thiccWAL = [] }

startOpts = StartOpts { detachKeys = DefaultDetachKey }
clientOpts = DockerClientOpts
  { apiVer = "v1.24"
  , baseUrl = "http://localhost:4243"
  }
grpcClientConf = Etcd.etcdClientConfigSimple "localhost" 2379 False
