module Model.DbPlayer.Query (
  getPlayersByRegion,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Model.DbPlayer.Types
import Model.Player.Adaptor (fromEntityToDbPlayer)

getPlayersByRegion :: Text -> Int -> IO [DbPlayer]
getPlayersByRegion db region =
  runSqlite db $ do
    players <- selectList [DbPlayerRegion ==. region] []

    liftIO $ return $ fmap fromEntityToDbPlayer players
