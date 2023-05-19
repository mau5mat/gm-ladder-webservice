{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Network (getGrandmastersFromRegion)

import Domain ( toPlayers
              , toPlayerInfo
              )

import Entities ( Player
                , PlayerInfo
                )

import DbEntities ( DbPlayer
                  , migrateDbEntity
                  )

import Control.Monad.IO.Class (liftIO)
import ConvertEntities (extractDbPlayerFromTuple)
import Database (insertDbPlayers)
import Data.Text (Text)

main :: IO ()
main = runApp

-- just testing here for now
runApp :: IO ()
runApp = do
  na <- getGrandmastersFromRegion "NA"

  let naPlayers = toPlayers na
  let naPlayerInfos = concatMap toPlayerInfo naPlayers
  let combinePlayerData = zip naPlayers naPlayerInfos
  let dbPlayers = fmap extractDbPlayerFromTuple combinePlayerData

  migrateDbEntity "players.sqlite3"

  insertDbPlayers "players.sqlite3" dbPlayers

  return ()
