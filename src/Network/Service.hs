module Network.Service (getPlayersFromRegion) where

import BattleNet (createUrlWithRegion)
import Data.Text (
  Text,
  unpack,
 )
import Model.LadderTeams.Types (LadderTeams)
import Model.Player.Adaptor (
  toDbPlayer,
  toDbPlayerFromTuple,
  toPlayerInfo,
  toPlayers,
 )
import Model.Player.Types (DbPlayer)
import Network.HTTP.Simple (
  Request,
  Response,
  getResponseBody,
  httpJSON,
  parseRequest_,
 )

getGrandmastersFromRegion :: Text -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Text -> Request
ladderTeamsRequestUrl regionId = parseRequest_ . unpack $ createUrlWithRegion regionId

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody

getPlayersFromRegion :: Text -> IO [DbPlayer]
getPlayersFromRegion region = do
  regionData <- getGrandmastersFromRegion region

  let players = toPlayers regionData
  let playerInfos = concatMap toPlayerInfo players
  let combinedPlayerData = zip players playerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  return dbPlayers
