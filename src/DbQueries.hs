{-# LANGUAGE OverloadedStrings #-}

module DbQueries (
  insertDbPlayers,
  getPlayersByRegion,
  deleteAllPlayers,
) where

import ConvertEntities (
  fromEntityToDbPlayer,
  toDbPlayerFromTuple,
  toPlayerInfo,
  toPlayers,
 )

import Entities (
  Player,
  PlayerInfo,
 )

import DbEntities (
  DbPlayer (..),
  EntityField (..),
  migrateDbEntity,
 )

import Domain (
  getPlayerByName,
  getPlayerHighestMmr,
  getPlayerWithHighestWinRate,
 )

import Network (getDataFromRegion)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)

getPlayersByRegion :: Text -> Int -> IO [DbPlayer]
getPlayersByRegion db region =
  runSqlite db $ do
    players <- selectList [DbPlayerRegion ==. region] []

    liftIO $ return $ fmap fromEntityToDbPlayer players

insertDbPlayers :: Text -> [DbPlayer] -> IO ()
insertDbPlayers db entities =
  runSqlite db $ do
    players <- mapM insert entities

    liftIO $ print players

deleteAllPlayers :: Text -> IO ()
deleteAllPlayers db =
  runSqlite db $ do
    deleteWhere ([] :: [Filter DbPlayer])

    liftIO $ putStr "deleted db players"

migrateAndInsertDbPlayers :: [DbPlayer] -> IO ()
migrateAndInsertDbPlayers dbPlayers = do
  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()

runRequest :: IO ()
runRequest = do
  krPlayers <- getDataFromRegion "KR"
  naPlayers <- getDataFromRegion "NA"
  euPlayers <- getDataFromRegion "EU"

  let allPlayers = krPlayers <> naPlayers <> euPlayers

  migrateAndInsertDbPlayers allPlayers

  return ()
