{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.KR (KrPlayersAPI, PlayersAPI, playersHandler) where

import qualified Model.DbPlayer.Query as Query
import qualified Model.DbPlayer.Service as Service

import App (App)
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.DbPlayer.Query (Region (..))
import Model.DbPlayer.Types (DbPlayer)
import Network.API.Config (appToHandler)
import Servant (Handler)
import Servant.API
import Servant.Server.Generic (AsServer)

-- Attempt at making Records style API with Servant

type KrPlayersAPI = NamedRoutes PlayersAPI

data PlayersAPI mode = PlayersAPI
  { players :: mode :- "gm-ladder" :> "eu" :> "players" :> Get '[JSON] [DbPlayer]
  , player :: mode :- "gm-ladder" :> "eu" :> "player" :> NamedRoutes PlayerAPI
  }
  deriving stock (Generic)

data PlayerAPI mode = PlayerAPI
  { name :: mode :- NamedRoutes PlayerNameAPI
  , highestWinRate :: mode :- "highest-win-rate" :> Get '[JSON] DbPlayer
  , highestMmr :: mode :- "highest-mmr" :> Get '[JSON] DbPlayer
  }
  deriving stock (Generic)

newtype PlayerNameAPI mode = PlayerNameAPI
  { namedPlayer :: mode :- Capture "name" PlayerName :> Get '[JSON] DbPlayer
  }
  deriving stock (Generic)

type PlayerName = Text

playersHandler :: PlayersAPI AsServer
playersHandler =
  PlayersAPI
    { players = allPlayers
    , player = playerHandler
    }

playerHandler :: PlayerAPI AsServer
playerHandler =
  PlayerAPI
    { name = playerNameHandler
    , highestWinRate = playerHighestWinrate
    , highestMmr = playerHighestMmr
    }

playerNameHandler :: PlayerNameAPI AsServer
playerNameHandler =
  PlayerNameAPI
    { namedPlayer = playerByName
    }

allPlayers :: Handler [DbPlayer]
allPlayers = do
  let s1 = Service.createService
      s2 = Query.createService

  appToHandler $ Service.getPlayers s1 s2 EU

playerByName :: PlayerName -> Handler DbPlayer
playerByName playerName = do
  let s1 = Service.createService
      s2 = Query.createService

  appToHandler $ Service.playerByName s1 s2 playerName EU

playerHighestWinrate :: Handler DbPlayer
playerHighestWinrate = do
  let s1 = Service.createService
      s2 = Query.createService

  appToHandler $ Service.playerHighestWinrate s1 s2 EU

playerHighestMmr :: Handler DbPlayer
playerHighestMmr = do
  let s1 = Service.createService
      s2 = Query.createService

  appToHandler $ Service.playerHighestMmr s1 s2 EU
