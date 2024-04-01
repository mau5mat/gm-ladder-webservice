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
import qualified Network.API.Routes.EU as EU (API, routes)
import qualified Network.API.Routes.KR as KR (API, routes)
import qualified Network.API.Routes.NA as NA (API, routes)
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

gmAPI :: Proxy API
gmAPI = Proxy

gmServer :: Server API
gmServer = hoistServer gmAPI appToHandler routes

routes :: ServerT API App
routes =
  NA.routes
    :<|> EU.routes
    :<|> KR.routes

type API = NA.API :<|> EU.API :<|> KR.API
