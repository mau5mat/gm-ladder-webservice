{-# LANGUAGE OverloadedStrings #-}

module Domain ( countRaceDistribution
              , numberOfTerrans
              , numberOfProtoss
              , numberOfZerg
              , getHighestMMRPlayer
              , getPlayerWithHighestWinRate
              ) where

import Entities ( LadderTeams(..)
                , Player(..)
                , PlayerInfo(..)
                )

import Data.Maybe ( catMaybes
                  , mapMaybe
                  , fromMaybe
                  )

import Data.Text (Text)


getPlayerWithHighestWinRate :: LadderTeams -> [Player]
getPlayerWithHighestWinRate ladder = highestWinPercentagePlayer
  where highestWinPercentagePlayer = filter (\player -> calculateWinPercentage (wins player) (losses player) == maximum percentageWinRateList) players
        percentageWinRateList      = fmap (\player -> calculateWinPercentage (wins player) (losses player)) players
        players                    = ladderTeams ladder

calculateWinPercentage :: Int -> Int -> String
calculateWinPercentage wins losses = show winPercent
  where totalPlayed = fromIntegral wins + fromIntegral losses
        winPercent  = fromIntegral wins / totalPlayed * 100

getHighestMMRPlayer :: LadderTeams -> [Player]
getHighestMMRPlayer ladder = highestMMRPlayer
  where highestMMRPlayer = filter (\player -> checkMMR player == maximum allMMRs) players
        allMMRs          = mapMaybe mmr players
        players          = ladderTeams ladder

checkMMR :: Player -> Int
checkMMR player = fromMaybe 0 (mmr player)

countRaceDistribution :: (Text -> Bool) -> LadderTeams -> Int
countRaceDistribution predicate ladder = length . filter predicate $ catMaybes races
  where races      = fmap favoriteRace playerInfo
        players    = ladderTeams ladder
        playerInfo = concatMap teamMembers players

numberOfTerrans :: LadderTeams -> Int
numberOfTerrans = countRaceDistribution isTerran

isTerran :: Text -> Bool
isTerran s =
  case s of
    "terran" -> True
    _        -> False

numberOfProtoss :: LadderTeams -> Int
numberOfProtoss = countRaceDistribution isProtoss

isProtoss :: Text -> Bool
isProtoss s =
  case s of
    "protoss" -> True
    _         -> False

numberOfZerg :: LadderTeams -> Int
numberOfZerg = countRaceDistribution isZerg

isZerg :: Text -> Bool
isZerg s =
  case s of
    "zerg" -> True
    _      -> False
