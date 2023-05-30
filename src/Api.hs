{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api ( runGmPort
           , runKrPort
           , naPlayerByName
           , naPlayerHighestMmr
           , naPlayerHighestWinrate
           ) where

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  )

import Domain ( getPlayerByName
              , getPlayerHighestMmr
              , getPlayerWithHighestWinRate
              )

import App ( AppT(..)
           , App
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

import Servant (throwError)

import DbQueries (getPlayersByRegion)


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

allGmRoutes :: ServerT GmApi App
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

naGmRoutes :: ServerT EuGmApi App
naGmRoutes
  = allNaPlayers
  :<|> naPlayerHighestWinrate
  :<|> naPlayerHighestMmr
  :<|> naPlayerByName

type NaGmApi
  = "gm-ladder" :> "na" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-win-rate" :> Get '[JSON] DbPlayer
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-mmr" :> Get '[JSON] DbPlayer
  :<|> "gm-ladder" :> "kr" :> "player" :> QueryParam' '[Required] "name" Text :> Get '[JSON] DbPlayer

allNaPlayers :: MonadIO m => AppT m [DbPlayer]
allNaPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  liftIO $ return players

naPlayerByName :: MonadIO m => Text -> AppT m DbPlayer
naPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p  -> return p

naPlayerHighestWinrate :: MonadIO m => AppT m DbPlayer
naPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p  -> return p

naPlayerHighestMmr :: MonadIO m => AppT m DbPlayer
naPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p  -> return p


runEuPort :: Port -> IO ()
runEuPort port = run port euApp

euApp :: Application
euApp = serve euGmAPI euServer

euGmAPI :: Proxy EuGmApi
euGmAPI = Proxy

euServer :: Server EuGmApi
euServer = hoistServer euGmAPI appToHandler euGmRoutes

euGmRoutes :: ServerT EuGmApi App
euGmRoutes
  = allEuPlayers
  :<|> euPlayerHighestWinrate
  :<|> euPlayerHighestMmr
  :<|> euPlayerByName

type EuGmApi
  = "gm-ladder" :> "eu" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-win-rate" :> Get '[JSON] DbPlayer
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-mmr" :> Get '[JSON] DbPlayer
  :<|> "gm-ladder" :> "kr" :> "player" :> QueryParam' '[Required] "name" Text :> Get '[JSON] DbPlayer

allEuPlayers :: MonadIO m => AppT m [DbPlayer]
allEuPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  liftIO$ return players

euPlayerByName :: MonadIO m => Text -> AppT m DbPlayer
euPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p  -> return p

euPlayerHighestWinrate :: MonadIO m => AppT m DbPlayer
euPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p  -> return p

euPlayerHighestMmr :: MonadIO m => AppT m DbPlayer
euPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p  -> return p


runKrPort :: Port -> IO ()
runKrPort port = run port krApp

krApp :: Application
krApp = serve krGmAPI krServer

krGmAPI :: Proxy KrGmApi
krGmAPI = Proxy

krServer :: Server KrGmApi
krServer = hoistServer krGmAPI appToHandler krGmRoutes

krGmRoutes :: ServerT KrGmApi App
krGmRoutes
  = allKrPlayers
  :<|> krPlayerHighestWinrate
  :<|> krPlayerHighestMmr
  :<|> krPlayerByName

type KrGmApi
  = "gm-ladder" :> "kr" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-win-rate" :> Get '[JSON] DbPlayer
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-mmr" :> Get '[JSON] DbPlayer
  :<|> "gm-ladder" :> "kr" :> "player" :> QueryParam' '[Required] "name" Text :> Get '[JSON] DbPlayer

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
    Just p  -> return p

krPlayerHighestWinrate :: App DbPlayer
krPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p  -> return p

krPlayerHighestMmr :: App DbPlayer
krPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p  -> return p
