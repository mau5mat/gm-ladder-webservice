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

module Model.DbPlayer.Types where

import App (App)
import Conduit (MonadIO (liftIO))
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Environment.Config as Config

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

migrateDbEntity :: App ()
migrateDbEntity =
  liftIO $ runSqlite Config.databaseName $ do
    runMigration migrateAll
    return ()
