{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.KR where

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

runKrPort :: Port -> IO ()
runKrPort port = run port krApp

krApp :: Application
krApp = serve krGmAPI krServer

krGmAPI :: Proxy KrGmApi
krGmAPI = Proxy

krServer :: Server KrGmApi
krServer = hoistServer krGmAPI appToHandler krGmRoutes

krGmRoutes :: ServerT KrGmApi App
krGmRoutes =
  allKrPlayers
    :<|> krPlayerHighestWinrate
    :<|> krPlayerHighestMmr
    :<|> krPlayerByName

type KrGmApi =
  "gm-ladder"
    :> "kr"
    :> "players"
    :> Get '[JSON] [DbPlayer]
    :<|> "gm-ladder"
      :> "kr"
      :> "player"
      :> "highest-win-rate"
      :> Get '[JSON] DbPlayer
    :<|> "gm-ladder"
      :> "kr"
      :> "player"
      :> "highest-mmr"
      :> Get '[JSON] DbPlayer
    :<|> "gm-ladder"
      :> "kr"
      :> "player"
      :> QueryParam' '[Required] "name" Text
      :> Get '[JSON] DbPlayer

allKrPlayers :: App [DbPlayer]
allKrPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3
  liftIO $ return players

krPlayerByName :: Text -> App DbPlayer
krPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

krPlayerHighestWinrate :: App DbPlayer
krPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

krPlayerHighestMmr :: App DbPlayer
krPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
