module Network (getDataFromRegion) where

import Data.Text ( Text
                 , unpack
                 )

import Network.HTTP.Simple( parseRequest_
                          , Request
                          , Response
                          , getResponseBody
                          , httpJSON
                          )

import ConvertEntities ( toPlayers
                       , toPlayerInfo
                       , toDbPlayerFromTuple
                       , fromEntityToDbPlayer
                       )

import BattleNet (createUrlWithRegion)

import Entities (LadderTeams)

import DbEntities (DbPlayer(..))


getGrandmastersFromRegion :: Text -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Text -> Request
ladderTeamsRequestUrl regionId = parseRequest_ . unpack $ createUrlWithRegion regionId

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody

getDataFromRegion :: Text -> IO [DbPlayer]
getDataFromRegion region = do
  regionData <- getGrandmastersFromRegion region

  let players = toPlayers regionData
  let playerInfos = concatMap toPlayerInfo players
  let combinedPlayerData = zip players playerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  return dbPlayers
