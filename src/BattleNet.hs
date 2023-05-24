{-# LANGUAGE OverloadedStrings #-}

module BattleNet (createUrlWithRegion) where

import Secrets (starcraftClientToken)


createUrlWithRegion :: String -> String
createUrlWithRegion id
  = baseUrl
  <> grandmasterPath
  <> regionIdToString id
  <> accessTokenParam
  <> starcraftClientToken

baseUrl :: String
baseUrl = "https://us.api.blizzard.com"

grandmasterPath :: String
grandmasterPath = "/sc2/ladder/grandmaster/"

accessTokenParam :: String
accessTokenParam = "?access_token="

regionIdToString :: String -> String
regionIdToString regionId =
  case regionId of
    "NA" -> "1"
    "EU" -> "2"
    "KR" -> "3"
    _    -> ""
