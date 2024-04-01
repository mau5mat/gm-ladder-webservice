module Network.Service (getPlayersFromRegion) where

import qualified Model.Player.Adaptor as Adaptor

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

getPlayersFromRegion :: Region -> IO [DbPlayer]
getPlayersFromRegion region = do
  regionData <- getGrandmastersFromRegion region

  let players = Adaptor.toPlayers regionData
  let playerInfos = concatMap Adaptor.toPlayerInfo players
  let combinedPlayerData = zip players playerInfos
  let dbPlayers = fmap Adaptor.toDbPlayerFromTuple combinedPlayerData

  return dbPlayers

getGrandmastersFromRegion :: Region -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Region -> Request
ladderTeamsRequestUrl region = parseRequest_ . unpack $ createUrlWithRegion region

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody
