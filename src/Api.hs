{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api where

import DbQueries ( getPlayerByName
                 , getPlayersByRegion
                 , getHighestMMRPlayer
                 , getPlayerWithHighestWinRate
                 )

import qualified Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import Servant.API
import Servant.Server

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  )

import Network.Wai
import Network.Wai.Handler.Warp

import GHC.Generics

--runKrServer :: Port -> IO ()
--runKrServer port = run port krApp

--krApp :: Application
--krApp = serve krGmAPI krGmServer

krGmServer :: Server KrGmApi
krGmServer
  = allKrPlayers :<|> krPlayerByName :<|> krPlayerHighestWinrate :<|> krPlayerHighestMmr

krGmAPI :: Proxy KrGmApi
krGmAPI = Proxy

type KrGmApi
  = "gm-ladder" :> "kr" :> "players" :> Get '[JSON] [DbPlayer]
  :<|> "gm-ladder" :> "kr" :> "player" :> Capture "name" Text :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-win-rate" :> Get '[JSON] (Maybe DbPlayer)
  :<|> "gm-ladder" :> "kr" :> "player" :> "highest-mmr" :> Get '[JSON] (Maybe DbPlayer)

allKrPlayers :: Handler [DbPlayer]
allKrPlayers = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return players

krPlayerByName :: Text -> Handler (Maybe DbPlayer)
krPlayerByName name = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return $ getPlayerByName players name

krPlayerHighestWinrate :: Handler (Maybe DbPlayer)
krPlayerHighestWinrate = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return $ getPlayerWithHighestWinRate players

krPlayerHighestMmr :: Handler (Maybe DbPlayer)
krPlayerHighestMmr = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 1
  return $ getHighestMMRPlayer players


--runNaServer :: Port -> IO ()
--runNaServer port = run port naApp

--naApp :: Application
--naApp = serve naGmAPI naGmServer

naGmServer :: Server NaGmApi
naGmServer = undefined

naGmAPI :: Proxy NaGmApi
naGmAPI = Proxy

type NaGmApi
  = "gm-ladder" :> "na" :> "players" :> Get '[JSON] [Entity DbPlayer]
  :<|> "gm-ladder" :> "na" :> "player" :> Capture "name" String :> Get '[JSON] (Entity DbPlayer)
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-win-rate" :> Get '[JSON] (Entity DbPlayer)
  :<|> "gm-ladder" :> "na" :> "player" :> "highest-mmr" :> Get '[JSON] (Entity DbPlayer)


--runEuServer :: Port -> IO ()
--runEuServer port = run port euApp

--euApp :: Application
--euApp = serve euGmAPI euGmServer

euGmServer :: Server EuGmApi
euGmServer = undefined

euGmAPI :: Proxy EuGmApi
euGmAPI = Proxy

type EuGmApi
  = "gm-ladder" :> "eu" :> "players" :> Get '[JSON] [Entity DbPlayer]
  :<|> "gm-ladder" :> "eu" :> "player" :> Capture "name" String :> Get '[JSON] (Entity DbPlayer)
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-win-rate" :> Get '[JSON] (Entity DbPlayer)
  :<|> "gm-ladder" :> "eu" :> "player" :> "highest-mmr" :> Get '[JSON] (Entity DbPlayer)
