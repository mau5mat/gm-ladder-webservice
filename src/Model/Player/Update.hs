{-# LANGUAGE OverloadedStrings #-}

module Model.Player.Update where

import qualified Environment.Config as Config

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist (Filter, deleteWhere, insert)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH ()
import Model.DbPlayer.Query (Region (..))
import Model.DbPlayer.Types (DbPlayer, migrateDbEntity)
import Network.Service (getPlayersFromRegion)

runRequest :: IO ()
runRequest = do
  krPlayers <- getPlayersFromRegion KR
  naPlayers <- getPlayersFromRegion NA
  euPlayers <- getPlayersFromRegion EU

  let allPlayers = krPlayers <> naPlayers <> euPlayers

  migrateAndInsertDbPlayers allPlayers

  return ()

insertDbPlayers :: [DbPlayer] -> IO ()
insertDbPlayers entities =
  runSqlite Config.databaseName $ do
    players <- mapM insert entities

    liftIO $ print players

deleteAllPlayers :: IO ()
deleteAllPlayers =
  runSqlite Config.databaseName $ do
    deleteWhere ([] :: [Filter DbPlayer])

    liftIO $ putStr "deleted db players"

migrateAndInsertDbPlayers :: [DbPlayer] -> IO ()
migrateAndInsertDbPlayers dbPlayers = do
  migrateDbEntity >> insertDbPlayers dbPlayers

  return ()
