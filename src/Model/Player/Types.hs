{-# LANGUAGE DeriveGeneric #-}

module Model.Player.Types where

import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import GHC.Generics (Generic)
import Model.PlayerInfo.Types (PlayerInfo)

data Player = Player
  { teamMembers :: [PlayerInfo]
  , previousRank :: Int
  , points :: Int
  , wins :: Int
  , losses :: Int
  , mmr :: Maybe Int
  , joinTimestamp :: Int
  }
  deriving (Generic, Show)
instance ToJSON Player
instance FromJSON Player
