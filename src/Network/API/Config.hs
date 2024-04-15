{-# LANGUAGE DataKinds #-}

module Network.API.Config where

import App (App)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Servant.Server (Handler (Handler))

appToHandler :: App a -> Handler a
appToHandler action = do
  v <- liftIO $ do
    runEff (runErrorNoCallStack action)
  liftEither v
