{-# LANGUAGE OverloadedStrings #-}

module Main where

import Domain ( getPlayerWithHighestWinRate
              , getPlayerHighestMmr
              )

import Api ( naPlayerByName
           , naPlayerHighestMmr
           , naPlayerHighestWinrate
           )

import DbQueries (getPlayersByRegion)

import Api ( runGmPort
           , runKrPort
           , naPlayerHighestMmr
           , naPlayerHighestWinrate
           )

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)

main :: IO ()
main = do
  --runKrPort 8081
  test

test :: IO ()
test = do
  players <- getPlayersByRegion "players.sqlite3" 3

  let player = getPlayerWithHighestWinRate players
  --let player = getPlayerHighestMmr players

  case player of
    Nothing -> print "Not working .."
    Just p  -> print p
