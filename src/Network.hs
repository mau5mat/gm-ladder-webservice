module Network (getGrandmastersFromRegion) where

import Data.Text ( Text
                 , unpack
                 )

import Entities (LadderTeams)

import BattleNet (createUrlWithRegion)

import Network.HTTP.Simple


getGrandmastersFromRegion :: Text -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Text -> Request
ladderTeamsRequestUrl regionId = parseRequest_ . unpack $ createUrlWithRegion regionId

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody
