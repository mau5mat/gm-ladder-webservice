{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.NA where

import App (
  App,
  AppT (..),
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Model.DbPlayer.Domain (
  getPlayerByName,
  getPlayerHighestMmr,
  getPlayerWithHighestWinRate,
 )
import Model.DbPlayer.Query (getPlayersByRegion)
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

runNaPort :: Port -> IO ()
runNaPort port = run port naApp

naApp :: Application
naApp = serve naGmAPI naServer

naGmAPI :: Proxy API
naGmAPI = Proxy

naServer :: Server API
naServer = hoistServer naGmAPI appToHandler routes

routes :: ServerT API App
routes =
  allNaPlayers
    :<|> naPlayerHighestWinrate
    :<|> naPlayerHighestMmr
    :<|> naPlayerByName

type API =
  Players
    :<|> HighestWinRate
    :<|> HighestMmr
    :<|> NamedPlayer

type Players =
  "gm-ladder"
    :> "na"
    :> "players"
    :> Get '[JSON] [DbPlayer]

type HighestWinRate =
  "gm-ladder"
    :> "na"
    :> "player"
    :> "highest-win-rate"
    :> Get '[JSON] DbPlayer

type HighestMmr =
  "gm-ladder"
    :> "na"
    :> "player"
    :> "highest-mmr"
    :> Get '[JSON] DbPlayer

type NamedPlayer =
  "gm-ladder"
    :> "na"
    :> "player"
    :> QueryParam' '[Required] "name" Text
    :> Get '[JSON] DbPlayer

allNaPlayers :: (MonadIO m) => AppT m [DbPlayer]
allNaPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  liftIO $ return players

naPlayerByName :: (MonadIO m) => Text -> AppT m DbPlayer
naPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

naPlayerHighestWinrate :: (MonadIO m) => AppT m DbPlayer
naPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

naPlayerHighestMmr :: (MonadIO m) => AppT m DbPlayer
naPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
