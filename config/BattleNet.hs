{-# LANGUAGE OverloadedStrings #-}

module BattleNet (createUrlWithRegion) where

import Data.Text (Text)
import Secrets (starcraftClientToken)

createUrlWithRegion :: Text -> Text
createUrlWithRegion region =
  baseUrl
    <> grandmasterPath
    <> regionIdToString region
    <> accessTokenParam
    <> starcraftClientToken

regionIdToString :: Text -> Text
regionIdToString regionId =
  case regionId of
    "NA" -> "1"
    "EU" -> "2"
    "KR" -> "3"
    _ -> ""

baseUrl :: Text
baseUrl = "https://us.api.blizzard.com"

grandmasterPath :: Text
grandmasterPath = "/sc2/ladder/grandmaster/"

accessTokenParam :: Text
accessTokenParam = "?access_token="
