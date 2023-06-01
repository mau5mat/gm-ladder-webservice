{-# LANGUAGE OverloadedStrings #-}

module DbQueries ( insertDbPlayers
                 , getPlayersByRegion
                 , deleteAllPlayers
                 ) where

import ConvertEntities ( toPlayers
                       , toPlayerInfo
                       , toDbPlayerFromTuple
                       , fromEntityToDbPlayer
                       )

import Entities ( Player
                , PlayerInfo
                )

import DbEntities ( DbPlayer(..)
                  , EntityField(..)
                  , migrateDbEntity
                  )

import Domain ( getPlayerByName
              , getPlayerHighestMmr
              , getPlayerWithHighestWinRate)

import Network (getGrandmastersFromRegion)

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)


getPlayersByRegion :: Text -> Int -> IO [DbPlayer]
getPlayersByRegion db region
  = runSqlite db $ do
  players <- selectList [DbPlayerRegion ==. region] []

  liftIO $ return $ fmap fromEntityToDbPlayer players

insertDbPlayers :: Text -> [DbPlayer] -> IO ()
insertDbPlayers db entities
  = runSqlite db $ do
    players <- mapM insert entities

    liftIO $ print players

deleteAllPlayers :: Text -> IO ()
deleteAllPlayers db
  = runSqlite db $ do
    deleteWhere ([] :: [Filter DbPlayer])

    liftIO $ print "deleted db players"

addNaData :: IO ()
addNaData = do
  na <- getGrandmastersFromRegion "NA"

  let naPlayers = toPlayers na
  let naPlayerInfos = concatMap toPlayerInfo naPlayers
  let combinedPlayerData = zip naPlayers naPlayerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()

addEuData :: IO ()
addEuData = do
  eu <- getGrandmastersFromRegion "EU"

  let euPlayers = toPlayers eu
  let euPlayerInfos = concatMap toPlayerInfo euPlayers
  let combinedPlayerData = zip euPlayers euPlayerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()

addKrData :: IO ()
addKrData = do
  kr <- getGrandmastersFromRegion "KR"

  let krPlayers = toPlayers kr
  let krPlayerInfos = concatMap toPlayerInfo krPlayers
  let combinedPlayerData = zip krPlayers krPlayerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()
