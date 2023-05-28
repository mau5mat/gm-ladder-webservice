{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api (runGmPort) where

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  )

import DbQueries (getPlayersByRegion)

import Domain ( getPlayerByName
              , getPlayerHighestMmr
              , getPlayerWithHighestWinRate
              )

import Servant.API
import Servant.Server

import Network.Wai.Handler.Warp ( Port
                                , run)
import Network.Wai

import Data.Text (Text)
import Data.Proxy

import Control.Monad.Reader (MonadIO)
import Control.Monad.IO.Class (liftIO)

import App (AppT(..))

import Servant (throwError)


appToHandler :: AppT IO a -> Handler a
appToHandler appT = Handler $ runApp appT


runGmPort :: Port -> IO ()
runGmPort port = run port gmApp

gmApp :: Application
gmApp = serve gmAPI gmServer

gmAPI :: Proxy GmApi
gmAPI = Proxy

gmServer :: Server GmApi
gmServer = hoistServer gmAPI appToHandler allGmRoutes

allGmRoutes :: MonadIO m => ServerT GmApi (AppT m)
allGmRoutes
  = naGmRoutes
  :<|> euGmRoutes
  :<|> krGmRoutes

type GmApi = NaGmApi :<|> EuGmApi :<|> KrGmApi


runNaPort :: Port -> IO ()
runNaPort port = run port naApp

naApp :: Application
naApp = serve naGmAPI naServer

naGmAPI :: Proxy NaGmApi
naGmAPI = Proxy

naServer :: Server NaGmApi
naServer = hoistServer naGmAPI appToHandler naGmRoutes

naGmRoutes :: MonadIO m => ServerT EuGmApi (AppT m)
naGmRoutes
  = allNaPlayers
  :<|> naPlayerByName
  :<|> naPlayerHighestWinrate
  :<|> naPlayerHighestMmr

type NaGmApi
  = "gm-ladder" :> "na" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "na" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allNaPlayers :: MonadIO m => AppT m [DbPlayer]
allNaPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return players

naPlayerByName :: MonadIO m => Text -> AppT m (Maybe DbPlayer)
naPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err401
    Just p  -> return $ Just p

naPlayerHighestWinrate :: MonadIO m => AppT m (Maybe DbPlayer)
naPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err401
    Just p  -> return $ Just p

naPlayerHighestMmr :: MonadIO m => AppT m (Maybe DbPlayer)
naPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x


runEuPort :: Port -> IO ()
runEuPort port = run port euApp

euApp :: Application
euApp = serve euGmAPI euServer

euGmAPI :: Proxy EuGmApi
euGmAPI = Proxy

euServer :: Server EuGmApi
euServer = hoistServer euGmAPI appToHandler euGmRoutes

euGmRoutes :: MonadIO m => ServerT EuGmApi (AppT m)
euGmRoutes
  = allEuPlayers
  :<|> euPlayerByName
  :<|> euPlayerHighestWinrate
  :<|> euPlayerHighestMmr

type EuGmApi
  = "gm-ladder" :> "eu" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "eu" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allEuPlayers :: MonadIO m => AppT m [DbPlayer]
allEuPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2
  return players

euPlayerByName :: MonadIO m => Text -> AppT m (Maybe DbPlayer)
euPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x


euPlayerHighestWinrate :: MonadIO m => AppT m (Maybe DbPlayer)
euPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x


euPlayerHighestMmr :: MonadIO m => AppT m (Maybe DbPlayer)
euPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x


runKrPort :: Port -> IO ()
runKrPort port = run port krApp

krApp :: Application
krApp = serve krGmAPI krServer


krGmAPI :: Proxy KrGmApi
krGmAPI = Proxy

krServer :: Server EuGmApi
krServer = hoistServer krGmAPI appToHandler krGmRoutes

krGmRoutes :: MonadIO m => ServerT KrGmApi (AppT m)
krGmRoutes
  = allKrPlayers
  :<|> krPlayerByName
  :<|> krPlayerHighestWinrate
  :<|> krPlayerHighestMmr

type KrGmApi
  = "gm-ladder" :> "kr" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "kr" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allKrPlayers :: MonadIO m => AppT m [DbPlayer]
allKrPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3
  return players

krPlayerByName :: MonadIO m => Text -> AppT m (Maybe DbPlayer)
krPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x


krPlayerHighestWinrate :: MonadIO m => AppT m (Maybe DbPlayer)
krPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x


krPlayerHighestMmr :: MonadIO m => AppT m (Maybe DbPlayer)
krPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err401
    Just x  -> return $ Just x
