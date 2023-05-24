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
                 , getPlayerByName
                 )

import Api (runKrServer)

import Data.Text (Text)

main :: IO ()
main = runKrServer 8081

-- just testing here for now
runApp :: IO ()
runApp = do
  --na <- getGrandmastersFromRegion "NA"

  --let naPlayers = toPlayers na
  --let naPlayerInfos = concatMap toPlayerInfo naPlayers
  --let combinedPlayerData = zip naPlayers naPlayerInfos
  --let dbPlayers = fmap toDbPlayerFromTuple combinedPlayerData

  --migrateDbEntity "players.sqlite3"

  --insertDbPlayers "players.sqlite3" dbPlayers

  getPlayersByRegion "players.sqlite3" 1

  --getPlayerByName "players.sqlite3" "MillForGG"

  return ()
