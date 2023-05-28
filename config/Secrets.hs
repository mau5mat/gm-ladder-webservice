{-# LANGUAGE OverloadedStrings #-}

module Secrets ( starcraftClientToken
               , dbName) where

import Data.Text (Text)


-- Use this to generate a token for the API calls, expires every 24hr
-- curl -u 4c5dd1ce08a248e1ada9a37928f96eac:Z53FCIeMfNwTaLZ55sQPHSGC2PNV1y62 -d grant_type=client_credentials https://oauth.battle.net/token

starcraftClientToken :: String
starcraftClientToken = "EUxZCBMTx147j0NvDxnT6Aqautj19xX5Qy"

dbName :: Text
dbName = "players.sqlite3"
