{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.EU where

import qualified Model.DbPlayer.Domain as Domain
import qualified Model.DbPlayer.Query as Query

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
  allEuPlayers
    :<|> euPlayerHighestWinrate
    :<|> euPlayerHighestMmr
    :<|> euPlayerByName

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

allEuPlayers :: App [DbPlayer]
allEuPlayers = do
  players <- liftIO $ Query.getPlayersByRegion EU

  liftIO $ return players

euPlayerByName :: Text -> App DbPlayer
euPlayerByName name = do
  players <- liftIO $ Query.getPlayersByRegion EU

  let player = Domain.getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

euPlayerHighestWinrate :: App DbPlayer
euPlayerHighestWinrate = do
  players <- liftIO $ Query.getPlayersByRegion EU

  let player = Domain.getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

euPlayerHighestMmr :: App DbPlayer
euPlayerHighestMmr = do
  players <- liftIO $ Query.getPlayersByRegion EU

  let player = Domain.getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
