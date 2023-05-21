{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Database ( runDb
                , insertDbPlayers
                ) where

import ConvertEntities ()

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Data.Text (Text)
import DbEntities (DbPlayer(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)

import Conduit (ResourceT, MonadUnliftIO)

-- setup and connect to getEntityFieldsDatabase

runDb :: Text -> IO ()
runDb db = runSqlite db $ do
  return ()

insertDbPlayers :: Text -> [DbPlayer] -> IO ()
insertDbPlayers db entities
  = runSqlite db $ do
    players <- mapM insert entities
    liftIO $ print players
