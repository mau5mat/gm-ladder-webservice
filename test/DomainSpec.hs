{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  , EntityField(..)
                  )

import Domain ( getPlayerByName
              , getPlayerHighestMmr
              , getPlayerWithHighestWinRate
              , countRaceDistribution
              , isTerran
              , isProtoss
              , isZerg
              )

import MockData (mockDbPlayers)

import Test.Hspec


spec :: Spec
spec = do
  describe "getPlayerByName" $ do
    it "returns a dbPlayer from a list of dbPlayers by matching a provided name to a name in the list" $ do
      let player = getPlayerByName mockDbPlayers "FlashFan"

      let flashFan
            = DbPlayer
            { dbPlayerPreviousRank = 2
            , dbPlayerPoints = 435
            , dbPlayerWins = 123
            , dbPlayerLosses = 12
            , dbPlayerMmr = Just 7000
            , dbPlayerJoinTimestamp = 12425391
            , dbPlayerRealm = 1
            , dbPlayerRegion = 1
            , dbPlayerDisplayName = "FlashFan"
            , dbPlayerClanTag = Just "gamers"
            , dbPlayerFavoriteRace = Just "terran"
            }

      case player of
        Nothing -> return ()
        Just p  -> p `shouldBe` flashFan

  describe "getPlayerHighestMmr" $ do
    it "returns a dbPlayer from a list of dbPlayers that has the highest mmr value" $ do
      let player = getPlayerHighestMmr mockDbPlayers

      let flashFan
            = DbPlayer
            { dbPlayerPreviousRank = 2
            , dbPlayerPoints = 435
            , dbPlayerWins = 123
            , dbPlayerLosses = 12
            , dbPlayerMmr = Just 7000
            , dbPlayerJoinTimestamp = 12425391
            , dbPlayerRealm = 1
            , dbPlayerRegion = 1
            , dbPlayerDisplayName = "FlashFan"
            , dbPlayerClanTag = Just "gamers"
            , dbPlayerFavoriteRace = Just "terran"
            }

      case player of
        Nothing -> return ()
        Just p  -> p `shouldBe` flashFan

  describe "getPlayerWithHighestWinRate" $ do
    it "returns a dbPlayer from a list of dbPlayers that has the highest calculated win rate from their wins and losses" $ do
      let player = getPlayerWithHighestWinRate mockDbPlayers

      let protossIdiot
           = DbPlayer
           { dbPlayerPreviousRank = 1
           , dbPlayerPoints = 465
           , dbPlayerWins = 120
           , dbPlayerLosses = 1
           , dbPlayerMmr = Just 6192
           , dbPlayerJoinTimestamp = 12425392
           , dbPlayerRealm = 1
           , dbPlayerRegion = 1
           , dbPlayerDisplayName = "protossidiot"
           , dbPlayerClanTag = Just ""
           , dbPlayerFavoriteRace = Just "protoss"
           }

      case player of
        Nothing -> return ()
        Just p  -> p `shouldBe` protossIdiot

  describe "countRaceDistribution" $ do
    it "returns the total amount players favour a certain race using a predicade" $ do
      let count = countRaceDistribution isTerran mockDbPlayers

      count `shouldBe` 1
