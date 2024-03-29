{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.All where

import App (
  App,
  AppT (..),
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.API.Config (appToHandler)
import Network.API.Routes.EU (EuGmApi, euGmRoutes)
import Network.API.Routes.KR (KrGmApi, krGmRoutes)
import Network.API.Routes.NA (NaGmApi, naGmRoutes)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Port,
  run,
 )
import Servant (throwError)
import Servant.API
import Servant.Server (Server, ServerT, hoistServer, serve)

runGmPort :: Port -> IO ()
runGmPort port = run port gmApp

gmApp :: Application
gmApp = serve gmAPI gmServer

gmAPI :: Proxy GmApi
gmAPI = Proxy

gmServer :: Server GmApi
gmServer = hoistServer gmAPI appToHandler allGmRoutes

allGmRoutes :: ServerT GmApi App
allGmRoutes =
  naGmRoutes
    :<|> euGmRoutes
    :<|> krGmRoutes

type GmApi = NaGmApi :<|> EuGmApi :<|> KrGmApi
