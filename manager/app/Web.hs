{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Web (webMain) where

import Thicc.Monad
import Thicc.Types

import Control.Monad.Except
import Control.Monad.Trans ( liftIO )
import Control.Concurrent.MVar
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Tuple ( swap )
import System.Directory ( doesFileExist, removeFile )
import System.FilePath ( FilePath, (</>) )

import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )

data ServiceCreated = ServiceCreated IPAddress

instance ToJSON ServiceCreated where
  toJSON (ServiceCreated ip) = object
    [ "ip" .= ip ]

data ScaleService = ScaleService Int

instance FromJSON ScaleService where
  parseJSON (Object o) =
    ScaleService <$> o .: "number"

type ThiccAPI =
  "services" :> Capture "id" T.Text :> (
  ReqBody '[JSON] CreateService
  :> Put '[JSON] ServiceCreated
  :<|>
  DeleteNoContent '[JSON] NoContent
  :<|>
  ReqBody '[JSON] ScaleService
  :> PostNoContent '[JSON] NoContent
  )
  :<|>
  "blob" :> Capture "name" T.Text :> (
  ReqBody '[OctetStream] LBS.ByteString
  :> PutNoContent '[JSON] NoContent
  :<|>
  DeleteNoContent '[JSON] NoContent
  )

thiccAPI :: Proxy ThiccAPI
thiccAPI = Proxy

server :: ThiccEnv -> MVar ThiccState -> Server ThiccAPI
server env svar = service :<|> blob where
  blob name = create :<|> delete where
    dest = envBlobDir env </> T.unpack name
    create bs = do
      b <- liftIO $ doesFileExist dest
      case b of
        True -> throwError err409 { errBody = "the file already exists" }
        False -> do
          liftIO $ LBS.writeFile dest bs
          pure NoContent

    delete = do
      b <- liftIO $ doesFileExist dest
      case b of
        True -> do
          liftIO $ removeFile dest
          pure NoContent
        False -> throwError err404 { errBody = "no such blob" }

  service sId = create :<|> delete :<|> scale where
    runThicc' :: Thicc a -> Handler (Either ThiccError a)
    runThicc' m = liftIO $ modifyMVar svar $ \st ->
      swap <$> runThicc env st m

    create (CreateService command) =
      runThicc' m >>=
      either interpretError (\(_, s) -> pure (ServiceCreated $ serviceProxyIP s))
      where
        m = do
          createService ServiceConfig
            { serviceConfigName = sId
            , serviceConfigCommand = command
            }

    delete =
      runThicc' (deleteService (ServiceId sId)) >>=
      either interpretError (const $ pure NoContent)

    scale (ScaleService n) =
      runThicc' m >>=
      either interpretError (const $ pure NoContent)
      where
        m = scaleService ScaleConfig
          { scaleConfigNumber = n
          , scaleConfigServiceId = ServiceId sId
          }

    interpretError :: ThiccError -> Handler a
    interpretError e = throwError $ case e of
      UnknownError s -> err err500 (T.pack s)
      NoSuchService (ServiceId sId) ->
        err err404 $ "no such service with id " <> sId
      NoSuchContainer name ->
        err err404 $ "no such container by name " <> name
      DockerError e ->
        err err500 $ "internal docker error " <> T.pack (show e)
      SaveFailed ->
        err err500 ("persisting to etcd failed" :: T.Text)
      InvariantViolated msg ->
        err err500 $ "internal invariant violated: " <> msg
      RefreshFailed msg ->
        err err500 $ "proxy refresh failed: " <> msg
      where
        err e msg = e { errBody = encode $ object [ "message" .= msg ] }

webMain :: ThiccEnv -> ThiccState -> IO ()
webMain env st = do
  svar <- newMVar st
  run 7133 $ logStdoutDev $ serve thiccAPI (server env svar)
