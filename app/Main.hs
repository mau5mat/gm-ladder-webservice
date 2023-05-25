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
              , getPlayerHighestMmr
              , getPlayerWithHighestWinRate)

import Api ( runKrServer
           , runNaServer
           , runEuServer
           )

import Data.Text (Text)

import Control.Monad.IO.Class (liftIO)


main :: IO ()
main = do
  players <- liftIO $ getPlayersByRegion "players.sqlite3" 2

  let player = getPlayerWithHighestWinRate players

  print player

runServers :: IO ()
runServers = do
    runKrServer 8081
    runNaServer 8081
    runEuServer 8081

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
