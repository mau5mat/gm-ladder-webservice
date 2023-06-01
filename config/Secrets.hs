{-# LANGUAGE OverloadedStrings #-}

module Secrets (starcraftClientToken) where

import Data.Text (Text)


-- Use this to generate a token for the API calls, expires every 24hr
-- curl -u 4c5dd1ce08a248e1ada9a37928f96eac:Z53FCIeMfNwTaLZ55sQPHSGC2PNV1y62 -d grant_type=client_credentials https://oauth.battle.net/token

starcraftClientToken :: Text
starcraftClientToken = "EUCx7y8Yss2qLvL4Wad33a5PmAADpVdCx3"
