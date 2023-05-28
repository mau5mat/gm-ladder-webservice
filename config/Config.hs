{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config ( App
              , AppT(..)
              ) where

import Control.Monad.Except ( MonadError
                            , ExceptT
                            )

import Control.Monad.Reader (MonadIO)

import Servant (ServerError)

import Data.Text (Text)


type App = AppT IO

newtype AppT m a
    = AppT
    { runApp :: (ExceptT ServerError m) a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ServerError
    , MonadIO
    )
