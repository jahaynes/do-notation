{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Routes.Signup where

import Errors
import Security.Security
import Storage.StorageApi
import Types.Json         (chop)
import Types.User         (UserId, RawPassword)

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except 
import Data.Aeson
import GHC.Generics               (Generic)

data Signup =
    Signup { s_username    :: !UserId
           , s_rawpassword :: !RawPassword
           } deriving Generic

instance FromJSON Signup where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeSignup :: SecurityApi IO -> StorageApi -> Signup -> ExceptT ErrorResponse IO ()
routeSignup securityApi storageApi (Signup uname pw) =
    -- TODO catchall too broad
    catchAll "Could not create new user.  Username already taken?" $ do
        (salt, hspw) <- lift (hashPassword securityApi pw)
        ExceptT (createPassword storageApi uname salt hspw)
