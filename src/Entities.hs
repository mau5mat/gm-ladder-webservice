{-# LANGUAGE DeriveGeneric #-}

module Entities ( LadderTeams(..)
                , Player(..)
                , PlayerInfo(..)
                ) where

import Data.Aeson ( ToJSON
                  , FromJSON
                  )

import GHC.Generics (Generic)

import Database.Persist.TH

import Data.Text (Text)


newtype LadderTeams
  = LadderTeams
  { ladderTeams :: [Player]
  } deriving (Generic, Show)
instance ToJSON LadderTeams
instance FromJSON LadderTeams

data Player
  = Player
  { teamMembers :: [PlayerInfo]
  , previousRank :: Int
  , points :: Int
  , wins :: Int
  , losses :: Int
  , mmr :: Maybe Int
  , joinTimestamp :: Int
  } deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player

data PlayerInfo
  = PlayerInfo
  { realm :: Int
  , region :: Int
  , displayName :: Text
  , clanTag :: Maybe Text
  , favoriteRace :: Maybe Text
  } deriving (Generic, Show)
instance ToJSON PlayerInfo
instance FromJSON PlayerInfo
