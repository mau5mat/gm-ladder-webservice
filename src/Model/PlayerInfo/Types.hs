{-# LANGUAGE DeriveGeneric #-}

module Model.PlayerInfo.Types where

import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Text (Text)
import GHC.Generics (Generic)

data PlayerInfo = PlayerInfo
  { realm :: Int
  , region :: Int
  , displayName :: Text
  , clanTag :: Maybe Text
  , favoriteRace :: Maybe Text
  }
  deriving (Generic, Show)
instance ToJSON PlayerInfo
instance FromJSON PlayerInfo
