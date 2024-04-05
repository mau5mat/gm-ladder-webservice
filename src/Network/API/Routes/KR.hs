{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.KR where

import qualified Model.DbPlayer.Query as Query
import qualified Model.DbPlayer.Service as Service

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Model.DbPlayer.Query (Region (..))
import Model.DbPlayer.Types (DbPlayer)
import Network.API.Config (appToHandler)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Port,
  run,
 )
import Servant (throwError)
import Servant.API
import Servant.Server (Server, ServerT, err404, hoistServer, serve)

runPort :: Port -> IO ()
runPort port = run port app

app :: Application
app = serve proxy server

proxy :: Proxy API
proxy = Proxy

server :: Server API
server = hoistServer proxy appToHandler routes

routes :: ServerT API App
routes =
  allPlayers
    :<|> playerHighestWinrate
    :<|> playerHighestMmr
    :<|> playerByName

type API =
  Players
    :<|> HighestWinRate
    :<|> HighestMmr
    :<|> NamedPlayer

type Players =
  "gm-ladder"
    :> "kr"
    :> "players"
    :> Get '[JSON] [DbPlayer]

type HighestWinRate =
  "gm-ladder"
    :> "kr"
    :> "player"
    :> "highest-win-rate"
    :> Get '[JSON] DbPlayer

type HighestMmr =
  "gm-ladder"
    :> "kr"
    :> "player"
    :> "highest-mmr"
    :> Get '[JSON] DbPlayer

type NamedPlayer =
  "gm-ladder"
    :> "kr"
    :> "player"
    :> QueryParam' '[Required] "name" Text
    :> Get '[JSON] DbPlayer

allPlayers :: App [DbPlayer]
allPlayers = do
  s1 <- Service.createService
  s2 <- Query.createService

  Service.getPlayers s1 s2 KR

playerByName :: Text -> App DbPlayer
playerByName name = do
  s1 <- Service.createService
  s2 <- Query.createService

  Service.playerByName s1 s2 name KR

playerHighestWinrate :: App DbPlayer
playerHighestWinrate = do
  s1 <- Service.createService
  s2 <- Query.createService

  Service.playerHighestWinrate s1 s2 KR

playerHighestMmr :: App DbPlayer
playerHighestMmr = do
  s1 <- Service.createService
  s2 <- Query.createService

  Service.playerHighestMmr s1 s2 KR
