{-# LANGUAGE OverloadedStrings #-}

module Model.Player.Update where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist (Filter, deleteWhere, insert)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH ()
import Model.Player.Types (DbPlayer, migrateDbEntity)
import Network.Service (getPlayersFromRegion)

runRequest :: IO ()
runRequest = do
  krPlayers <- getPlayersFromRegion "KR"
  naPlayers <- getPlayersFromRegion "NA"
  euPlayers <- getPlayersFromRegion "EU"

  let allPlayers = krPlayers <> naPlayers <> euPlayers

  migrateAndInsertDbPlayers allPlayers

  return ()

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
