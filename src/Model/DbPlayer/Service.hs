module Model.DbPlayer.Service (
  allPlayers,
  playerByName,
  playerHighestWinrate,
  playerHighestMmr,
) where

import qualified Environment.Config as Config

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
import Model.DbPlayer.Query (Region, getPlayersByRegion)
import Model.DbPlayer.Types (DbPlayer (..))
import Network.API.Config (appToHandler)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
  Port,
  run,
 )
import Servant (throwError)
import Servant.API
import Servant.Server (Server, ServerT, err404, hoistServer, serve)

allPlayers :: Region -> App [DbPlayer]
allPlayers region = do
  players <- liftIO $ getPlayersByRegion region

  liftIO $ return players

playerByName :: Region -> Text -> App DbPlayer
playerByName region name = do
  players <- liftIO $ getPlayersByRegion region

  let player = getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

playerHighestWinrate :: Region -> App DbPlayer
playerHighestWinrate region = do
  players <- liftIO $ getPlayersByRegion region

  let player = getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

playerHighestMmr :: Region -> App DbPlayer
playerHighestMmr region = do
  players <- liftIO $ getPlayersByRegion region

  let player = getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
