{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Maybe

import Servant.Types.SourceT (source)
import Servant.API

import ApiEntities (ApiPlayer(..))

import GHC.Generics


type KrGmApi = "players" :> Get '[JSON] [ApiPlayer]
type NaGmApi = "players" :> Get '[JSON] [ApiPlayer]
type EuGmApi = "players" :> Get '[JSON] [ApiPlayer]
