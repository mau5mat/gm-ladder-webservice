module Network.API.Config where

import App (AppT (runApp))
import Servant.Server (Handler (Handler))

appToHandler :: AppT IO a -> Handler a
appToHandler appT = Handler $ runApp appT
