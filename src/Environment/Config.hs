{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Environment.Config where

import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics (Generic)

databaseName :: Text
databaseName = "players.sqlite3"

data ConnectionInfo = ConnectionInfo
  { connectHost :: Text
  , connectPort :: Word16
  , connectUser :: Text
  , connectPassword :: Text
  , connectDatabase :: Text
  , sslMode :: Text
  }
  deriving (Generic, Eq, Read, Show)
