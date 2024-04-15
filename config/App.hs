{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module App (
  App,
  runApp,
) where

import Control.Monad.IO.Class (liftIO)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Servant (ServerError)

type App = Eff '[Error ServerError, IOE]

runApp :: App a -> IO (Either ServerError a)
runApp = liftIO . runEff . runErrorNoCallStack @ServerError
