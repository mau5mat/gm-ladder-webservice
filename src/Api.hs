{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api ( runKrServer
           , runNaServer
           , runEuServer
           ) where

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

import Control.Monad.IO.Class (liftIO)


runKrServer :: Port -> IO ()
runKrServer port = run port krApp

krApp :: Application
krApp = serve krGmAPI krGmServer

krGmServer :: Server KrGmApi
krGmServer
  = allKrPlayers
  :<|> krPlayerByName
  :<|> krPlayerHighestWinrate
  :<|> krPlayerHighestMmr

krGmAPI :: Proxy KrGmApi
krGmAPI = Proxy

type KrGmApi
  = "gm-ladder" :> "kr" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "kr" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allKrPlayers :: Handler [DbPlayer]
allKrPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3
  return players

krPlayerByName :: Text -> Handler (Maybe DbPlayer)
krPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3
  return $ getPlayerByName players name

krPlayerHighestWinrate :: Handler (Maybe DbPlayer)
krPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3
  return $ getPlayerWithHighestWinRate players

krPlayerHighestMmr :: Handler (Maybe DbPlayer)
krPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 3
  return $ getPlayerHighestMmr players


runNaServer :: Port -> IO ()
runNaServer port = run port naApp

naApp :: Application
naApp = serve naGmAPI naGmServer

naGmServer :: Server NaGmApi
naGmServer
  = allNaPlayers
  :<|> naPlayerByName
  :<|> naPlayerHighestWinrate
  :<|> naPlayerHighestMmr

naGmAPI :: Proxy NaGmApi
naGmAPI = Proxy

type NaGmApi
  = "gm-ladder" :> "na" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "na" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allNaPlayers :: Handler [DbPlayer]
allNaPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return players

naPlayerByName :: Text -> Handler (Maybe DbPlayer)
naPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return $ getPlayerByName players name

naPlayerHighestWinrate :: Handler (Maybe DbPlayer)
naPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return $ getPlayerWithHighestWinRate players

naPlayerHighestMmr :: Handler (Maybe DbPlayer)
naPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return $ getPlayerHighestMmr players


runEuServer :: Port -> IO ()
runEuServer port = run port euApp

euApp :: Application
euApp = serve euGmAPI euGmServer

euGmServer :: Server EuGmApi
euGmServer
  = allEuPlayers
  :<|> euPlayerByName
  :<|> euPlayerHighestWinrate
  :<|> euPlayerHighestMmr

euGmAPI :: Proxy EuGmApi
euGmAPI = Proxy

type EuGmApi
  = "gm-ladder" :> "eu" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "eu" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allEuPlayers :: Handler [DbPlayer]
allEuPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2
  return players

euPlayerByName :: Text -> Handler (Maybe DbPlayer)
euPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2
  return $ getPlayerByName players name

euPlayerHighestWinrate :: Handler (Maybe DbPlayer)
euPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2
  return $ getPlayerWithHighestWinRate players

euPlayerHighestMmr :: Handler (Maybe DbPlayer)
euPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2
  return $ getPlayerHighestMmr players
