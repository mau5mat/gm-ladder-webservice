{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.All where

import qualified Network.API.Routes.EU as EU
import qualified Network.API.Routes.KR as KR
import qualified Network.API.Routes.NA as NA

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Port,
  run,
 )
import Servant (throwError)
import Servant.API
import Servant.Server (Server, ServerT, hoistServer, serve)
import Servant.Server.Generic (AsServer)

runPort :: Port -> App ()
runPort port = liftIO $ run port app

api :: Proxy API
api = Proxy @API

app :: Application
app = _ _ _

server :: API AsServer
server =
  API
    { america = NA.playersHandler
    , europe = EU.playersHandler
    , korea = KR.playersHandler
    }

data API mode = API
  { america :: mode :- NA.NaPlayersAPI
  , europe :: mode :- EU.EuPlayersAPI
  , korea :: mode :- KR.KrPlayersAPI
  }
  deriving stock (Generic)
