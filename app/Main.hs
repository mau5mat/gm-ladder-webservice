module Main where

import Data.Text (Text)
import Network.API.Routes.All (runGmPort)

main :: IO ()
main = do
  runGmPort 8081
