{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.EU where

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
    :> "eu"
    :> "players"
    :> Get '[JSON] [DbPlayer]

type HighestWinRate =
  "gm-ladder"
    :> "eu"
    :> "player"
    :> "highest-win-rate"
    :> Get '[JSON] DbPlayer

type HighestMmr =
  "gm-ladder"
    :> "eu"
    :> "player"
    :> "highest-mmr"
    :> Get '[JSON] DbPlayer

type NamedPlayer =
  "gm-ladder"
    :> "eu"
    :> "player"
    :> QueryParam' '[Required] "name" Text
    :> Get '[JSON] DbPlayer

allPlayers :: App [DbPlayer]
allPlayers = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.getPlayers s1 s2 EU

playerByName :: Text -> App DbPlayer
playerByName name = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.playerByName s1 s2 name EU

playerHighestWinrate :: App DbPlayer
playerHighestWinrate = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.playerHighestWinrate s1 s2 EU

playerHighestMmr :: App DbPlayer
playerHighestMmr = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.playerHighestMmr s1 s2 EU
