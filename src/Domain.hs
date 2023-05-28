{-# LANGUAGE OverloadedStrings          #-}

module Domain ( getPlayerByName
              , getPlayerWithHighestWinRate
              , getPlayerHighestMmr
              ) where

import DbEntities (DbPlayer(..))

import Data.Maybe ( mapMaybe
                  , fromMaybe
                  , catMaybes
                  )

import Data.Text (Text)


getPlayerByName :: [DbPlayer] -> Text -> Maybe DbPlayer
getPlayerByName dbPlayers name
  = case namedPlayer of
      [] -> Nothing
      [x] -> Just x
  where namedPlayer
          = filter (\player -> dbPlayerDisplayName player == name) dbPlayers

getPlayerHighestMmr :: [DbPlayer] -> Maybe DbPlayer
getPlayerHighestMmr dbPlayers
  = case highestMMRPlayer of
      []  -> Nothing
      [x] -> Just x
  where highestMMRPlayer
          = filter (\player -> checkMMR player == maximum allMMRs) dbPlayers
        allMMRs
          = mapMaybe dbPlayerMmr dbPlayers

getPlayerWithHighestWinRate :: [DbPlayer] -> Maybe DbPlayer
getPlayerWithHighestWinRate dbPlayers
  = case highestWinPercentagePlayer of
      []  -> Nothing
      [x] -> Just x
  where highestWinPercentagePlayer
          = filter (\player -> calculateWinPercentage (dbPlayerWins player) (dbPlayerLosses player) == maximum percentageWinRateList) dbPlayers
        percentageWinRateList
          = fmap (\player -> calculateWinPercentage (dbPlayerWins player) (dbPlayerLosses player)) dbPlayers

calculateWinPercentage :: Int -> Int -> String
calculateWinPercentage wins losses = show winPercent
  where totalPlayed
          = fromIntegral wins + fromIntegral losses
        winPercent
          = fromIntegral wins / totalPlayed * 100

checkMMR :: DbPlayer -> Int
checkMMR dbPlayer = fromMaybe 0 (dbPlayerMmr dbPlayer)

countRaceDistribution :: (Text -> Bool) -> [DbPlayer] -> Int
countRaceDistribution predicate dbPlayers
  = length . filter predicate $ catMaybes races
  where races
          = fmap dbPlayerFavoriteRace dbPlayers

numberOfTerrans :: [DbPlayer] -> Int
numberOfTerrans = countRaceDistribution isTerran

isTerran :: Text -> Bool
isTerran s =
  case s of
    "terran" -> True
    _        -> False

numberOfProtoss :: [DbPlayer] -> Int
numberOfProtoss = countRaceDistribution isProtoss

isProtoss :: Text -> Bool
isProtoss s =
  case s of
    "protoss" -> True
    _         -> False

numberOfZerg :: [DbPlayer] -> Int
numberOfZerg = countRaceDistribution isZerg

isZerg :: Text -> Bool
isZerg s =
  case s of
    "zerg" -> True
    _      -> False
