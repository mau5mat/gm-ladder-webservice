{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App (
  App,
  AppT (..),
) where

import Control.Monad.Except (
  ExceptT,
  MonadError,
 )

import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import Servant (ServerError)

type App = AppT IO

newtype AppT m a = AppT
  { runApp :: (ExceptT ServerError m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ServerError
    , MonadIO
    )
