{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.All where

import qualified Network.API.Routes.EU as EU
import qualified Network.API.Routes.KR as KR
import qualified Network.API.Routes.NA as NA

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.API.Config (appToHandler)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Port,
  run,
 )
import Servant (throwError)
import Servant.API
import Servant.Server (Server, ServerT, hoistServer, serve)

runPort :: Port -> App ()
runPort port = liftIO $ run port app

app :: Application
app = serve proxy server

proxy :: Proxy API
proxy = Proxy

server :: Server API
server = hoistServer proxy appToHandler routes

routes :: ServerT API App
routes =
  NA.routes
    :<|> EU.routes
    :<|> KR.routes

type API =
  NA.API
    :<|> EU.API
    :<|> KR.API
