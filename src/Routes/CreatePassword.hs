{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Routes.CreatePassword where

import Errors
import Security.Security
import Storage.StorageApi
import Types.Json         (chop)
import Types.User         (UserId, RawPassword)

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except 
import Data.Aeson
import GHC.Generics               (Generic)

data CreatePassword =
    CreatePassword { cu_username    :: !UserId
                   , cu_rawpassword :: !RawPassword
                   } deriving Generic

instance FromJSON CreatePassword where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeCreatePassword :: SecurityApi IO -> StorageApi -> CreatePassword -> ExceptT ErrorResponse IO ()
routeCreatePassword securityApi storageApi (CreatePassword uname pw) =
    -- TODO catchall too broad
    catchAll "Could not create new user.  Username already taken?" $ do
        (salt, hspw) <- lift (hashPassword securityApi pw)
        ExceptT (createPassword storageApi uname salt hspw)
