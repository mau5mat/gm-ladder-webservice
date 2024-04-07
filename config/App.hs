{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App (
  App,
  AppT (..),
  runApp,
) where

import Control.Monad.Except (
  ExceptT,
  MonadError,
  runExceptT,
 )

import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import Servant (ServerError)

type App = AppT IO

newtype AppT m a = AppT ((ExceptT ServerError m) a)

runApp :: App a -> IO (Either ServerError a)
runApp (AppT stack) = runExceptT stack

-- newtype AppT m a = AppT
-- { runApp :: (ExceptT ServerError m) a
-- }
-- deriving
-- ( Functor
-- , Applicative
-- , Monad
-- , MonadError ServerError
-- , MonadIO
-- )
