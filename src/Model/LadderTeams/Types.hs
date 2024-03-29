{-# LANGUAGE DeriveGeneric #-}

module Model.LadderTeams.Types where

import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Text (Text)
import Database.Persist.TH ()
import GHC.Generics (Generic)
import Model.Player.Types (Player)

newtype LadderTeams = LadderTeams
  { ladderTeams :: [Player]
  }
  deriving (Generic, Show)
instance ToJSON LadderTeams
instance FromJSON LadderTeams
