{-# LANGUAGE OverloadedStrings #-}

module Model.DbPlayer.Query (
  Region (..),
  Service (..),
  createService,
  regionToInt,
  regionToText,
) where

import qualified Environment.Config as Config

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist (selectList, (==.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH ()
import Model.DbPlayer.Adaptor (fromEntityToDbPlayer)
import Model.DbPlayer.Types

newtype Service = Service
  { getPlayersByRegion :: Region -> IO [DbPlayer]
  }

createService :: IO Service
createService = do
  pure
    Service
      { getPlayersByRegion = getPlayersByRegion_
      }

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

getPlayersByRegion_ :: Region -> IO [DbPlayer]
getPlayersByRegion_ region =
  runSqlite Config.databaseName $ do
    let playerRegion = regionToInt region

    players <- selectList [DbPlayerRegion ==. playerRegion] []

    let dbPlayers = fmap fromEntityToDbPlayer players

    return dbPlayers
