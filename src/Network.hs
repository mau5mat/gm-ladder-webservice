module Network (getGrandmastersFromRegion) where

import Entities (LadderTeams)

import BattleNet (createUrlWithRegion)

import Network.HTTP.Simple

getGrandmastersFromRegion :: String -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: String -> Request
ladderTeamsRequestUrl regionId = parseRequest_ $ createUrlWithRegion regionId

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody
