{-# LANGUAGE OverloadedStrings #-}

module BattleNet (createUrlWithRegion) where

import Data.Text (Text)
import Model.DbPlayer.Query (Region (..), regionToText)
import Secrets (starcraftClientToken)

createUrlWithRegion :: Region -> Text
createUrlWithRegion region =
  baseUrl
    <> grandmasterPath
    <> regionToText region
    <> accessTokenParam
    <> starcraftClientToken

baseUrl :: Text
baseUrl = "https://us.api.blizzard.com"

grandmasterPath :: Text
grandmasterPath = "/sc2/ladder/grandmaster/"

accessTokenParam :: Text
accessTokenParam = "?access_token="
