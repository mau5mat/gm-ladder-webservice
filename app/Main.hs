module Main where

import App (runApp)
import Conduit (MonadIO (liftIO))
import Model.Player.Update (runRequests)
import Network.API.Routes.All (runPort)
import Servant.Server (ServerError)

import qualified Model.DbPlayer.Domain as Domain
import qualified Model.DbPlayer.Query as Query
import qualified Network.Service as Network

main :: IO (Either ServerError ())
main = runApp $ do
  let s1 = Network.createService
  let s2 = Query.createService

  euPlayers <- Query.getPlayersByRegion s2 Query.EU

  liftIO $ print (Domain.getPlayerHighestMmr euPlayers)

  -- _ <- runRequests s1
  -- _ <- runPort 8081

  return ()
