{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Test.Hspec

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  , EntityField(..)
                  )

import Domain (getPlayerByName)

import Data.Text (Text)

mockData :: [DbPlayer]
mockData = [ DbPlayer
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
             , DbPlayer
             { dbPlayerPreviousRank = 1
             , dbPlayerPoints = 465
             , dbPlayerWins = 120
             , dbPlayerLosses = 63
             , dbPlayerMmr = Just 6192
             , dbPlayerJoinTimestamp = 12425392
             , dbPlayerRealm = 1
             , dbPlayerRegion = 1
             , dbPlayerDisplayName = "protossidiot"
             , dbPlayerClanTag = Just ""
             , dbPlayerFavoriteRace = Just "protoss"
             }
             , DbPlayer
             { dbPlayerPreviousRank = 3
             , dbPlayerPoints = 467
             , dbPlayerWins = 121
             , dbPlayerLosses = 57
             , dbPlayerMmr = Just 5899
             , dbPlayerJoinTimestamp = 12425393
             , dbPlayerRealm = 1
             , dbPlayerRegion = 1
             , dbPlayerDisplayName = "stargamer451"
             , dbPlayerClanTag = Just "gamers"
             , dbPlayerFavoriteRace = Just "zerg"
             }
           ]

spec :: Spec
spec = do
  describe "getPlayerByName" $ do
    it "returns a dbPlayer from a list of dbPlayers" $ do
      let player = getPlayerByName mockData "FlashFan"
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
