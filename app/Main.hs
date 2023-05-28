

module Main where

import Api (runGmPort)


main :: IO ()
main = do
  runGmPort 8081
