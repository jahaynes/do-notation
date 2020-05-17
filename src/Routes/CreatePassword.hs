{-# LANGUAGE DeriveGeneric #-}

module Routes.CreatePassword where

import Security.Security
import Storage.StorageApi
import Types.Json         (chop)
import Types.User         (UserId, RawPassword)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics           (Generic)
import Servant                (Handler)

data CreatePassword =
    CreatePassword { cu_username    :: !UserId
                   , cu_rawpassword :: !RawPassword
                   } deriving Generic

instance FromJSON CreatePassword where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeCreatePassword :: SecurityApi IO -> StorageApi -> CreatePassword -> Handler ()
routeCreatePassword securityApi storageApi (CreatePassword uname pw) =
    liftIO $ do
        (salt, hspw) <- hashPassword securityApi pw
        createPassword storageApi uname salt hspw
