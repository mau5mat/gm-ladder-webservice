module Model.DbPlayer.Service (
  allPlayers,
  playerByName,
  playerHighestWinrate,
  playerHighestMmr,
) where

import qualified Environment.Config as Config
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
  players <- liftIO $ Query.getPlayersByRegion region

  liftIO $ return players

playerByName :: Region -> Text -> App DbPlayer
playerByName region name = do
  players <- liftIO $ Query.getPlayersByRegion region

  let player = Domain.getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

playerHighestWinrate :: Region -> App DbPlayer
playerHighestWinrate region = do
  players <- liftIO $ Query.getPlayersByRegion region

  let player = Domain.getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

playerHighestMmr :: Region -> App DbPlayer
playerHighestMmr region = do
  players <- liftIO $ Query.getPlayersByRegion region

  let player = Domain.getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
