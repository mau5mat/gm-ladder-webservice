module Main where

import App (runApp)
import Model.Player.Update (runRequests)
import Network.API.Routes.All (runPort)
import Network.Service (createService)
import Servant.Server (ServerError)

main :: IO (Either ServerError ())
main = runApp $ do
  let service = createService

  _ <- runRequests service
  _ <- runPort 8081

  return ()
