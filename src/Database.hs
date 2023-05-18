
module Database ( runDb ) where

import ConvertEntities ()

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Data.Text (Text)
import DbEntities (DbPlayer(..))

import Control.Monad.IO.Class (liftIO)

-- setup and connect to getEntityFieldsDatabase

runDb :: Text -> IO ()
runDb db = runSqlite db $ do
  return ()

insertDbPlayers :: Text -> [DbPlayer] -> IO ()
insertDbPlayers db entities
  = runSqlite db $ do
    players <- mapM insert entities
    liftIO $ print players

deletePlayer :: Text -> DbPlayer -> IO ()
deletePlayer db dbPlayer
  = runSqlite db $ do
    return ()
