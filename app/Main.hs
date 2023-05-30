module Main where

import Domain ( getPlayerWithHighestWinRate
              , getPlayerHighestMmr
              )

import Api ( runGmPort
           , runKrPort
           , naPlayerHighestMmr
           , naPlayerHighestWinrate
           )

import DbQueries (getPlayersByRegion)

import Data.Text (Text)

main :: IO ()
main = do
  runKrPort 8081
