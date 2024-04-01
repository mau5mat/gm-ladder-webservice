{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.API.Routes.NA where

import qualified Model.DbPlayer.Domain as Domain
import qualified Model.DbPlayer.Query as Query

import App (
  App,
  AppT (..),
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
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

allNaPlayers :: App [DbPlayer]
allNaPlayers = do
  players <- liftIO $ Query.getPlayersByRegion NA
  liftIO $ return players

naPlayerByName :: Text -> App DbPlayer
naPlayerByName name = do
  players <- liftIO $ Query.getPlayersByRegion NA

  let player = Domain.getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

naPlayerHighestWinrate :: App DbPlayer
naPlayerHighestWinrate = do
  players <- liftIO $ Query.getPlayersByRegion NA

  let player = Domain.getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

naPlayerHighestMmr :: App DbPlayer
naPlayerHighestMmr = do
  players <- liftIO $ Query.getPlayersByRegion NA

  let player = Domain.getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
