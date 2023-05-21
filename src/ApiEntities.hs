module ApiEntities (ApiPlayer(..)) where

import Data.Text (Text)


data ApiPlayer
  = ApiPlayer
  { apiPreviousRank :: Int
  , apiPoints :: Int
  , apiWins :: Int
  , apiLosses :: Int
  , apiMmr :: Maybe Int
  , apiJoinTimestamp :: Int
  , apiRealm :: Int
  , apiRegion :: Int
  , apiDisplayName :: Text
  , apiClanTag :: Maybe Text
  , apiFavoriteRace :: Maybe Text
  } deriving (Show)
