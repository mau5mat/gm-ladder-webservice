{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Player.Types where

import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Model.PlayerInfo.Types (PlayerInfo)

data Player = Player
  { teamMembers :: [PlayerInfo]
  , previousRank :: Int
  , points :: Int
  , wins :: Int
  , losses :: Int
  , mmr :: Maybe Int
  , joinTimestamp :: Int
  }
  deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
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
    deriving Show Eq
|]

migrateDbEntity :: Text -> IO ()
migrateDbEntity db = runSqlite db $ do
  runMigration migrateAll
  return ()
