module Main where

import qualified Network.Service as Network

import App (App, runApp)
import Control.Monad.IO.Class (liftIO)
import Model.Player.Update (runRequests)
import Network.API.Routes.All (runPort)

main :: IO ()
main = do
  putStrLn "Starting App.."
  let service = Network.createService

  putStrLn "Running Requests.."
  -- runRequests service

  putStrLn "Running Port.."

  -- runPort 8081
  return ()
