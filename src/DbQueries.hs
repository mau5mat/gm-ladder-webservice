{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module DbQueries ( insertDbPlayers
                 , getPlayersByRegion
                 , getPlayerByName
                 , deletePlayer
                 ) where

import ConvertEntities ()

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Data.Text (Text)
import DbEntities ( DbPlayer(..)
                  , EntityField(..)
                  )

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)

import Conduit (ResourceT, MonadUnliftIO)


insertDbPlayers :: Text -> [DbPlayer] -> IO ()
insertDbPlayers db entities
  = runSqlite db $ do
    players <- mapM insert entities
    liftIO $ print players

getPlayersByRegion :: Text -> Int -> IO ()
getPlayersByRegion db region
  = runSqlite db $ do
  players <- selectList [DbPlayerRegion ==. 1] []
  liftIO $ print players

getPlayerByName :: Text -> Text -> IO ()
getPlayerByName db name
  = runSqlite db $ do
  players <- selectList [DbPlayerDisplayName ==. name] []
  liftIO $ print players


deletePlayer :: Text -> DbPlayer -> IO ()
deletePlayer db dbPlayer
  = runSqlite db $ do
    return ()
