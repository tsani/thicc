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
import Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import Data.Tuple ( swap )

import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )

data CreateService = CreateService Command

instance FromJSON CreateService where
  parseJSON (Object o) =
    CreateService <$> o .: "command"

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

thiccAPI :: Proxy ThiccAPI
thiccAPI = Proxy

server :: ThiccEnv -> MVar ThiccState -> Server ThiccAPI
server env svar sId = create :<|> delete :<|> scale where
  runThicc' :: Thicc a -> Handler (Either ThiccError a)
  runThicc' m = liftIO $ modifyMVar svar $ \st -> 
    swap <$> runThicc env st m

  create (CreateService command) = do
    e <- runThicc' $ do
      createService ServiceConfig
        { serviceConfigName = sId
        , serviceConfigCommand = command
        }
    case e of
      Left e -> throwError err500
      Right (_, s) -> pure (ServiceCreated $ serviceProxyIP s)
  delete = do
    e <- runThicc' $ deleteService (ServiceId sId)
    case e of
      Left e -> throwError err500
      Right x -> pure NoContent

  scale (ScaleService n) = do
    e <- runThicc' $ scaleService ScaleConfig
      { scaleConfigNumber = n
      , scaleConfigServiceId = ServiceId sId
      }
    case e of
      Left e -> throwError err500
      Right x -> pure NoContent

webMain :: ThiccEnv -> ThiccState -> IO ()
webMain env st = do
  svar <- newMVar st
  run 7133 $ logStdoutDev $ serve thiccAPI (server env svar)
