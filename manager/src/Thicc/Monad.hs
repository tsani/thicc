{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Thicc.Monad where

import Thicc.Types
import qualified Thicc.WAL as WAL

import Data.Coerce
import qualified Data.Map as M
import Control.Monad ( when )
import Control.Monad.Reader
import Control.Monad.Except
import Docker.Client

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

data ThiccError =
  UnknownError String

type ServiceMap = M.Map ServiceId Service

-- | The persistent state of the manager.
data ThiccState = ThiccState
  { thiccServiceMap :: ServiceMap
  , thiccWAL :: [WAL.LogEntry]
  }

processLogEntry :: WAL.LogEntry -> ServiceMap -> Thicc ServiceMap
processLogEntry entry map = _

processLog :: Thicc ()
processLog = _

addLogEntry :: WAL.LogEntry -> Thicc ()
addLogEntry = _
  
-- | The concrete Thicc monad, which implements 'MonadThicc'.
newtype Thicc a = Thicc
  { unThicc :: ExceptT ThiccError (ReaderT ThiccEnv IO) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadThicc Thicc where
  createService config = do
    addLogEntry (WAL.CreateService config)
    processLog

    _
  scaleService config = do
    let serviceId = scaleConfigServiceId config
    workerIds <- map (unWorkerId . workerId) <$> asks serviceWorkers
    let workerCount = length workerIds
    let baseId =
          case workerIds of
            [] -> 0
            _  -> maximum workerIds
    let delta = scaleConfigNumber config - workerCount
    if delta < 0 then do
      let delta' = negate delta
      let idsToKill = take delta' workerIds

      addLogEntry (WAL.ProxyRefresh serviceId (Just $ drop delta' (coerce workerIds)))
      forM_ idsToKill $ \idToKill ->
        addLogEntry $ WAL.KillWorker serviceId (WorkerId idToKill)
    else do
      forM_ [1 .. delta] $ \_ ->
        addLogEntry $ WAL.BootWorker serviceId
      when (delta /= 0) $ addLogEntry (WAL.ProxyRefresh serviceId Nothing)

    processLog
    _

  deleteService serviceId = do
    addLogEntry (WAL.DeleteService serviceId)
    processLog
    _
  
runThicc :: ThiccEnv -> Thicc a -> IO a
runThicc env (Thicc r) = runReaderT r env
  
-- | The environment used to run the 'Thicc' monad.
data ThiccEnv = ThiccEnv
  { envHttpHandler :: HttpHandler IO
  , envStartOpts :: StartOpts
  , envClientOpts :: DockerClientOpts
  }

-- | Constructs a default 'ThiccEnv'.
-- This requires IO in order to instantiate an HTTP handler.
mkThiccEnv :: IO ThiccEnv
mkThiccEnv = do
  h <- defaultHttpHandler
  pure ThiccEnv
    { envHttpHandler = h
    , envStartOpts = startOpts
    , envClientOpts = clientOpts
    }

startOpts = StartOpts { detachKeys = DefaultDetachKey }
clientOpts = DockerClientOpts
  { apiVer = "v1.24"
  , baseUrl = "http://localhost:4243"
  }

