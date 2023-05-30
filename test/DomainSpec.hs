{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  , EntityField(..)
                  )

import Domain (getPlayerByName)

import MockData (mockDbPlayers)

import Test.Hspec


spec :: Spec
spec = do
  describe "getPlayerByName" $ do
    it "returns a dbPlayer from a list of dbPlayers" $ do
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
