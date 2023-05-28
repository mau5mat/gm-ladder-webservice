{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Network (getGrandmastersFromRegion)

import ConvertEntities ( toPlayers
                       , toPlayerInfo
                       , toDbPlayerFromTuple
                       )

import Entities ( Player
                , PlayerInfo
                )

import DbEntities ( DbPlayer
                  , migrateDbEntity
                  )

import DbQueries ( insertDbPlayers
                 , getPlayersByRegion
                 )

import Domain ( getPlayerByName
              , getPlayerByName_
              , getPlayerHighestMmr
              , getPlayerWithHighestWinRate)

import Api ( runKrPort
           , runNaPort
           , runEuPort
           )

import Config (AppT(..))

import Data.Text (Text)

import Control.Monad.Reader (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Servant (throwError, err401)


main :: IO ()
main = return ()

runTests :: AppT IO DbPlayer
runTests = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerByName_ players "Clem"

  case player of
    Nothing -> throwError err401
    Just x  -> return x

runServers :: IO ()
runServers = do
    runKrPort 8081
    runNaPort 8081
    runEuPort 8081

naData :: IO ()
naData = do
  na <- getGrandmastersFromRegion "NA"

  let naPlayers = toPlayers na
  let naPlayerInfos = concatMap toPlayerInfo naPlayers
  let combinedPlayerData = zip naPlayers naPlayerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()

euData :: IO ()
euData = do
  eu <- getGrandmastersFromRegion "EU"

  let euPlayers = toPlayers eu
  let euPlayerInfos = concatMap toPlayerInfo euPlayers
  let combinedPlayerData = zip euPlayers euPlayerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()

krData :: IO ()
krData = do
  kr <- getGrandmastersFromRegion "KR"

  let krPlayers = toPlayers kr
  let krPlayerInfos = concatMap toPlayerInfo krPlayers
  let combinedPlayerData = zip krPlayers krPlayerInfos
  let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  migrateDbEntity "players.sqlite3"
  insertDbPlayers "players.sqlite3" dbPlayers

  return ()
