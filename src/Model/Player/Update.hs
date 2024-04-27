{-# LANGUAGE OverloadedStrings #-}

module Model.Player.Update (runRequests) where

import qualified Environment.Config as Config
import qualified Network.Service as Network

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.Array (run)
import Database.Persist (Filter, deleteWhere, insert)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH ()
import Model.DbPlayer.Query (Region (..))
import Model.DbPlayer.Types (DbPlayer, migrateDbEntity)

runRequests :: Network.Service -> App ()
runRequests service = do
  krPlayers <- Network.getPlayersFromRegion service KR
  naPlayers <- Network.getPlayersFromRegion service NA
  euPlayers <- Network.getPlayersFromRegion service EU

  let allPlayers = krPlayers <> naPlayers <> euPlayers

  migrateAndInsertDbPlayers allPlayers

  return ()

migrateAndInsertDbPlayers :: [DbPlayer] -> App ()
migrateAndInsertDbPlayers dbPlayers = do
  migrateDbEntity >> insertDbPlayers dbPlayers

  return ()

insertDbPlayers :: [DbPlayer] -> App ()
insertDbPlayers entities =
  liftIO $ runSqlite Config.databaseName $ do
    players <- mapM insert entities

    liftIO $ print players

deleteAllPlayers :: App ()
deleteAllPlayers =
  liftIO $ runSqlite Config.databaseName $ do
    deleteWhere ([] :: [Filter DbPlayer])

    liftIO $ putStr "deleted db players"
