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
                  , Entity(..)
                  , EntityField(..)
                  , migrateDbEntity
                  ) where

import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Text (Text)


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
