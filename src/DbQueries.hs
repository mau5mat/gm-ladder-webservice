module DbQueries ( insertDbPlayers
                 , getPlayersByRegion
                 ) where

import ConvertEntities (fromEntityToDbPlayer)

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Control.Monad.IO.Class (liftIO)

import DbEntities ( DbPlayer(..)
                  , EntityField(..)
                  )

import Data.Text (Text)


getPlayersByRegion :: Text -> Int -> IO [DbPlayer]
getPlayersByRegion db region
  = runSqlite db $ do
  players <- selectList [DbPlayerRegion ==. region] []

  liftIO $ return $ fmap fromEntityToDbPlayer players

insertDbPlayers :: Text -> [DbPlayer] -> IO ()
insertDbPlayers db entities
  = runSqlite db $ do
    players <- mapM insert entities

    liftIO $ print players
