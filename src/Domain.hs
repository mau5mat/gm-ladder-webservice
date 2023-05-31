{-# LANGUAGE OverloadedStrings #-}

module Domain ( getPlayerByName
              , getPlayerWithHighestWinRate
              , getPlayerHighestMmr
              , countRaceDistribution
              , isTerran
              , isProtoss
              , isZerg
              ) where

import DbEntities (DbPlayer(..))

import Data.List ( find
                 , maximumBy
                 )

import Data.Maybe ( mapMaybe
                  , fromMaybe
                  , catMaybes
                  )

import Data.Function (on)

import Data.Text (Text)


getPlayerByName :: [DbPlayer] -> Text -> Maybe DbPlayer
getPlayerByName dbPlayers name
  = find (\player -> dbPlayerDisplayName player == name) dbPlayers

getPlayerHighestMmr :: [DbPlayer] -> Maybe DbPlayer
getPlayerHighestMmr [] = Nothing
getPlayerHighestMmr xs = Just (maximumBy (compare `on` checkMMR) xs)
  where
  checkMMR :: DbPlayer -> Int
  checkMMR dbPlayer = fromMaybe 0 (dbPlayerMmr dbPlayer)

getPlayerWithHighestWinRate :: [DbPlayer] -> Maybe DbPlayer
getPlayerWithHighestWinRate [] = Nothing
getPlayerWithHighestWinRate xs = Just (maximumBy (compare `on` playerWinPercentage) xs)
  where
  playerWinPercentage p
    = calculateWinPercentage (dbPlayerWins p) (dbPlayerLosses p)
  calculateWinPercentage wins losses
    = show (winPercent :: Double)
    where
      totalPlayed
        = fromIntegral wins + fromIntegral losses
      winPercent
        = fromIntegral wins / totalPlayed * 100

countRaceDistribution :: (Text -> Bool) -> [DbPlayer] -> Int
countRaceDistribution predicate dbPlayers
  = length . filter predicate $ catMaybes races
  where
    races
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
