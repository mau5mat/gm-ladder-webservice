{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module DbEntities ( DbPlayer(..)
                  , migrateDbEntity
                  , getPlayerByName
                  , getPlayersByRegion
                  ) where

import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

-- Database Models

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbPlayer json
    previousRank Int
    points Int
    wins Int
    losses Int
    mmr Int Maybe
    joinTimestamp Int
    realm Int
    region Int
    displayName Text
    clanTag Text Maybe
    favoriteRace Text Maybe
    deriving Show
|]

migrateDbEntity :: Text -> IO ()
migrateDbEntity db = runSqlite db $ do
  runMigration migrateAll
  return ()

deletePlayer :: Text -> DbPlayer -> IO ()
deletePlayer db dbPlayer
  = runSqlite db $ do
    return ()

getPlayerByName :: Text -> Text -> IO ()
getPlayerByName db name
  = runSqlite db $ do
  players <- selectList [DbPlayerDisplayName ==. name] []
  liftIO $ print players

getPlayersByRegion :: Text -> Int -> IO ()
getPlayersByRegion db region
  = runSqlite db $ do
  players <- selectList [DbPlayerRegion ==. 1] []
  liftIO $ print players
