module Main where

import Data.Text (Text)
import Network.API.Routes.All (runPort)

main :: IO ()
main = do
  runPort 8081
