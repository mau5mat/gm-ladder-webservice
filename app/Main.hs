module Main where

import Network (getGrandmastersFromRegion)

import Domain ( toPlayers
              , toPlayerInfo
              )

import Control.Monad.IO.Class (liftIO)

import ConvertEntities (toDbPlayer)

import Database ()

--import

main :: IO ()
main = runApp

-- just testing here for now
runApp :: IO ()
runApp = do
  na <- getGrandmastersFromRegion "NA"

  let naPlayers = toPlayers na
  let naPlayerInfos = concatMap toPlayerInfo naPlayers

  print naPlayers
