module Main where

import App (App)
import Conduit (MonadIO (liftIO))
import Data.Text (Text)
import Model.Player.Update (runRequests)
import Network.API.Routes.All (runPort)
import Network.Service (createService)
import Servant (serveWithContext)

main :: IO ()
main = do
  putStrLn "Starting App.."
  let service = createService

  putStrLn "Running Requests.."
  let _ = runRequests service

  putStrLn "Running Port.."
  -- runPort 8081
  return ()
