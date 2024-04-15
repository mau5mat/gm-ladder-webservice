module Network.Service (createService, Service (..)) where

import qualified Model.LadderTeams.Adaptor as LadderTeamsAdaptor
import qualified Model.Player.Adaptor as PlayerAdaptor

import App (App, runApp)
import BattleNet (createUrlWithRegion)
import Data.Text (
  Text,
  unpack,
 )
import Model.DbPlayer.Query (Region (..))
import Model.DbPlayer.Types (DbPlayer)
import Model.LadderTeams.Types (LadderTeams)
import Network.HTTP.Simple (
  Request,
  Response,
  getResponseBody,
  httpJSON,
  parseRequest_,
 )

newtype Service = Service
  { getPlayersFromRegion :: Region -> App [DbPlayer]
  }

createService :: Service
createService =
  Service
    { getPlayersFromRegion = getPlayersFromRegion_
    }

getPlayersFromRegion_ :: Region -> App [DbPlayer]
getPlayersFromRegion_ region = do
  regionData <- getGrandmastersFromRegion region

  let players = LadderTeamsAdaptor.toPlayers regionData
      playerInfos = concatMap PlayerAdaptor.toPlayerInfo players
      combinedPlayerData = zip players playerInfos
      dbPlayers = fmap PlayerAdaptor.toDbPlayerFromTuple combinedPlayerData

  return dbPlayers

getGrandmastersFromRegion :: Region -> App LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Region -> Request
ladderTeamsRequestUrl region = parseRequest_ . unpack $ createUrlWithRegion region

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody
