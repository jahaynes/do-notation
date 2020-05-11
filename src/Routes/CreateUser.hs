module Routes.CreateUser where

import Requests.CreateUser
import Security.Security
import Storage.StorageApi

import Control.Monad.IO.Class (liftIO)
import Servant                (Handler)

routeCreateUser :: SecurityApi IO -> StorageApi -> CreateUser -> Handler ()
routeCreateUser securityApi storageApi (CreateUser uname pw) =
    liftIO $ do
        (salt, hspw) <- hashPassword securityApi pw
        createUser storageApi uname salt hspw