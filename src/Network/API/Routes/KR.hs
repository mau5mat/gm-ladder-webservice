{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.KR where

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
  allKrPlayers
    :<|> krPlayerHighestWinrate
    :<|> krPlayerHighestMmr
    :<|> krPlayerByName

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

allKrPlayers :: App [DbPlayer]
allKrPlayers = do
  players <- liftIO $ Query.getPlayersByRegion KR
  liftIO $ return players

krPlayerByName :: Text -> App DbPlayer
krPlayerByName name = do
  players <- liftIO $ Query.getPlayersByRegion KR

  let player = Domain.getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

krPlayerHighestWinrate :: App DbPlayer
krPlayerHighestWinrate = do
  players <- liftIO $ Query.getPlayersByRegion KR

  let player = Domain.getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

krPlayerHighestMmr :: App DbPlayer
krPlayerHighestMmr = do
  players <- liftIO $ Query.getPlayersByRegion KR

  let player = Domain.getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
