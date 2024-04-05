{-# LANGUAGE OverloadedStrings #-}

module DbQueriesSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import ConvertEntities (fromEntityToDbPlayer)
import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import DbEntities (
  DbPlayer (..),
  Entity (..),
  EntityField (..),
  migrateDbEntity,
 )
import DbQueries (
  deleteAllPlayers,
  getPlayersByRegion,
  insertDbPlayers,
 )
import Domain (getPlayerByName)
import Test.Hspec (
  Spec,
  context,
  describe,
  it,
  shouldBe,
 )

import Control.Monad (join)

spec :: Spec
spec = do
  describe "insertDbPlayers" $ do
    it "inserts a list of entities into a sqlite database" $ do
      migrateDbEntity "test_db.sqlite3"

      liftIO $ insertDbPlayers "test_db.sqlite3" mockDbPlayers

  describe "getPlayersByRegion" $ do
    it "returns a list of dbPlayers from a provided region" $ do
      players <- getPlayersByRegion "test_db.sqlite3" 1

      let player = getPlayerByName players "FlashFan"

      let flashFan =
            DbPlayer
              { dbPlayerPreviousRank = 2
              , dbPlayerPoints = 435
              , dbPlayerWins = 123
              , dbPlayerLosses = 12
              , dbPlayerMmr = Just 7000
              , dbPlayerJoinTimestamp = 12425391
              , dbPlayerRealm = 1
              , dbPlayerRegion = 1
              , dbPlayerDisplayName = "FlashFan"
              , dbPlayerClanTag = Just "gamers"
              , dbPlayerFavoriteRace = Just "terran"
              }

      case player of
        Nothing -> return ()
        Just p -> p `shouldBe` flashFan

    it "removes all players from a database" $ do
      deleteAllPlayers "test_db.sqlite3"
