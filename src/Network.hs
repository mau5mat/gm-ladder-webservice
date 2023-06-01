module Network (getGrandmastersFromRegion) where

import Data.Text ( Text
                 , unpack
                 )

import Network.HTTP.Simple( parseRequest_
                          , Request
                          , Response
                          , getResponseBody
                          , httpJSON
                          )

import BattleNet (createUrlWithRegion)

import Entities (LadderTeams)


getGrandmastersFromRegion :: Text -> IO LadderTeams
getGrandmastersFromRegion region = do
  response <- httpJSON $ ladderTeamsRequestUrl region

  pure $ decodeLadderTeams response

ladderTeamsRequestUrl :: Text -> Request
ladderTeamsRequestUrl regionId = parseRequest_ . unpack $ createUrlWithRegion regionId

decodeLadderTeams :: Response LadderTeams -> LadderTeams
decodeLadderTeams = getResponseBody
