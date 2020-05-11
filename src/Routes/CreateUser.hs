{-# LANGUAGE DeriveGeneric #-}

module Routes.CreateUser where

import Security.Security
import Storage.StorageApi
import Types.Json         (chop)
import Types.User         (UserId, RawPassword)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics           (Generic)
import Servant                (Handler)

data CreateUser =
    CreateUser { cu_username    :: !UserId
               , cu_rawpassword :: !RawPassword
               } deriving Generic

instance FromJSON CreateUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeCreateUser :: SecurityApi IO -> StorageApi -> CreateUser -> Handler ()
routeCreateUser securityApi storageApi (CreateUser uname pw) =
    liftIO $ do
        (salt, hspw) <- hashPassword securityApi pw
        createUser storageApi uname salt hspw
