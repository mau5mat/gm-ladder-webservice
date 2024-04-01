{-# LANGUAGE OverloadedStrings #-}

module Model.DbPlayer.Query (
  Region (..),
  getPlayersByRegion,
  regionToInt,
  regionToText,
) where

import qualified Environment.Config as Config

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Model.DbPlayer.Types
import Model.Player.Adaptor (fromEntityToDbPlayer)

data Region
  = NA
  | EU
  | KR

regionToInt :: Region -> Int
regionToInt region =
  case region of
    NA -> 1
    EU -> 2
    KR -> 3

regionToText :: Region -> Text
regionToText region =
  case region of
    NA -> "1"
    EU -> "2"
    KR -> "3"

getPlayersByRegion :: Region -> IO [DbPlayer]
getPlayersByRegion r =
  runSqlite Config.databaseName $ do
    let region = regionToInt r

    players <- selectList [DbPlayerRegion ==. region] []

    let dbPlayers = fmap fromEntityToDbPlayer players

    return dbPlayers
