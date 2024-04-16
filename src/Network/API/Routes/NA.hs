{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.NA where

import qualified Model.DbPlayer.Query as Query
import qualified Model.DbPlayer.Service as Service

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Model.DbPlayer.Query (Region (..))
import Model.DbPlayer.Types (DbPlayer)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Port,
  run,
 )
import Servant (throwError)
import Servant.API
import Servant.Server (Server, ServerT, err404, hoistServer, serve)

-- Attempt at making Records style API with Servant
--

type EuPlayersAPI = NamedRoutes PlayersAPI

data PlayersAPI mode = PlayersAPI
  { players :: mode :- "gm-ladder" :> "na" :> "players" :> Get '[JSON] [DbPlayer]
  , player :: mode :- "gm-ladder" :> "na" :> "player" :> NamedRoutes PlayerAPI
  }
  deriving stock (Generic)

data PlayerAPI mode = PlayerAPI
  { namedPlayer :: mode :- QueryParam' '[Required] "name" Text :> Get '[JSON] DbPlayer
  , highestWinRate :: mode :- "highest-win-rate" :> Get '[JSON] DbPlayer
  , highestMmr :: mode :- "highest-mmr" :> Get '[JSON] DbPlayer
  }
  deriving stock (Generic)

routes :: ServerT API App
routes =
  allPlayers
    :<|> playerHighestWinrate
    :<|> playerHighestMmr
    :<|> playerByName

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

allPlayers :: App [DbPlayer]
allPlayers = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.getPlayers s1 s2 NA

playerByName :: Text -> App DbPlayer
playerByName name = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.playerByName s1 s2 name NA

playerHighestWinrate :: App DbPlayer
playerHighestWinrate = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.playerHighestWinrate s1 s2 NA

playerHighestMmr :: App DbPlayer
playerHighestMmr = do
  let s1 = Service.createService
      s2 = Query.createService

  Service.playerHighestMmr s1 s2 NA
