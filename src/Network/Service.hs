module Network.Service (createService, Service (..)) where

import qualified Model.LadderTeams.Adaptor as LadderTeamsAdaptor
import qualified Model.Player.Adaptor as PlayerAdaptor

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
  { getPlayersFromRegion :: Region -> IO [DbPlayer]
  }

createService :: IO Service
createService = do
  pure
    Service
      { getPlayersFromRegion = getPlayersFromRegion_
      }

getPlayersFromRegion_ :: Region -> IO [DbPlayer]
getPlayersFromRegion_ region = do
  regionData <- getGrandmastersFromRegion region

  let players = LadderTeamsAdaptor.toPlayers regionData
  let playerInfos = concatMap PlayerAdaptor.toPlayerInfo players
  let combinedPlayerData = zip players playerInfos
  let dbPlayers = fmap PlayerAdaptor.toDbPlayerFromTuple combinedPlayerData

  return dbPlayers

getGrandmastersFromRegion :: Region -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Region -> Request
ladderTeamsRequestUrl region = parseRequest_ . unpack $ createUrlWithRegion region

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody
