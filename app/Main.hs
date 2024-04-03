module Main where

import Data.Text (Text)
import Model.Player.Update (runRequest)
import Network.API.Routes.All (runPort)
import Network.Service (createService)
import Servant (serveWithContext)

main :: IO ()
main = do
  putStrLn "Starting App.."
  service <- createService
  putStrLn "Running Request.."
  runRequest service

-- runPort 8081
