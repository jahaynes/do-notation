{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Routes.Signup (Signup, routeSignup) where

import Errors
import Security.Security
import Storage.StorageApi
import Types.Json         (chop)
import Types.User         (UserId (..), RawPassword)

import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Except 
import           Data.Aeson
import qualified Data.Text                  as T
import           GHC.Generics                    (Generic)

data Signup =
    Signup { s_username    :: !UserId
           , s_rawpassword :: !RawPassword
           } deriving Generic

instance FromJSON Signup where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeSignup :: SecurityApi IO -> StorageApi -> Signup -> ExceptT ErrorResponse IO ()
routeSignup securityApi storageApi (Signup userId pw) = do

    validate userId

    -- TODO catchall too broad
    catchAll "Could not create new user.  Username already taken?" $ do
        (salt, hspw) <- lift (hashPassword securityApi pw)
        ExceptT (createPassword storageApi userId salt hspw)

    where
    validate :: UserId -> ExceptT ErrorResponse IO ()
    validate (UserId ui)
        | ":" `T.isInfixOf` ui = err 400 "Username cannot contain ':'"
        | T.null ui            = err 400 "Username was empty"
        | T.length ui > 40     = err 400 "Username too long (40+)"
        | otherwise            = pure ()