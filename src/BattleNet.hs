{-# LANGUAGE OverloadedStrings #-}

module BattleNet (createUrlWithRegion) where

import Secrets (starcraftClientToken)

import Data.Text (Text)

createUrlWithRegion :: Text -> Text
createUrlWithRegion region
  = baseUrl
  <> grandmasterPath
  <> regionIdToString region
  <> accessTokenParam
  <> starcraftClientToken

baseUrl :: Text
baseUrl = "https://us.api.blizzard.com"

grandmasterPath :: Text
grandmasterPath = "/sc2/ladder/grandmaster/"

accessTokenParam :: Text
accessTokenParam = "?access_token="

regionIdToString :: Text -> Text
regionIdToString regionId =
  case regionId of
    "NA" -> "1"
    "EU" -> "2"
    "KR" -> "3"
    _    -> ""
