module Model.DbPlayer.Service (Service (..), createService) where

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

data Service = Service
  { getPlayers :: Query.Service -> Region -> App [DbPlayer]
  , playerByName :: Query.Service -> Region -> Text -> App DbPlayer
  , playerHighestWinrate :: Query.Service -> Region -> App DbPlayer
  , playerHighestMmr :: Query.Service -> Region -> App DbPlayer
  }

createService :: IO Service
createService =
  pure
    Service
      { getPlayers = getPlayers_
      , playerByName = playerByName_
      , playerHighestWinrate = playerHighestWinrate_
      , playerHighestMmr = playerHighestMmr_
      }

getPlayers_ :: Query.Service -> Region -> App [DbPlayer]
getPlayers_ service region = do
  players <- liftIO $ Query.getPlayersByRegion service region

  liftIO $ return players

playerByName_ :: Query.Service -> Region -> Text -> App DbPlayer
playerByName_ service region name = do
  players <- liftIO $ Query.getPlayersByRegion service region

  let player = Domain.getPlayerByName players name

  case player of
    Nothing -> throwError err404
    Just p -> return p

playerHighestWinrate_ :: Query.Service -> Region -> App DbPlayer
playerHighestWinrate_ service region = do
  players <- liftIO $ Query.getPlayersByRegion service region

  let player = Domain.getPlayerWithHighestWinRate players

  case player of
    Nothing -> throwError err404
    Just p -> return p

playerHighestMmr_ :: Query.Service -> Region -> App DbPlayer
playerHighestMmr_ service region = do
  players <- liftIO $ Query.getPlayersByRegion service region

  let player = Domain.getPlayerHighestMmr players

  case player of
    Nothing -> throwError err404
    Just p -> return p
