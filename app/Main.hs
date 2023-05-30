module Main where

import Domain ( getPlayerWithHighestWinRate
              , getPlayerHighestMmr
              )

import Api ( runGmPort
           , naPlayerHighestMmr
           , naPlayerHighestWinrate
           )

import DbQueries (getPlayersByRegion)

import Data.Text (Text)

main :: IO ()
main = do
  runGmPort 8081
