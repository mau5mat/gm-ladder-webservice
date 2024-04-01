{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.EU where

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

runEuPort :: Port -> IO ()
runEuPort port = run port euApp

euApp :: Application
euApp = serve euGmAPI euServer

euGmAPI :: Proxy API
euGmAPI = Proxy

euServer :: Server API
euServer = hoistServer euGmAPI appToHandler routes

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

allEuPlayers :: (MonadIO m) => AppT m [DbPlayer]
allEuPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  liftIO $ return players

euPlayerByName :: (MonadIO m) => Text -> AppT m DbPlayer
euPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

euPlayerHighestWinrate :: (MonadIO m) => AppT m DbPlayer
euPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

euPlayerHighestMmr :: (MonadIO m) => AppT m DbPlayer
euPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
