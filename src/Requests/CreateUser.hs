{-# LANGUAGE DeriveGeneric #-}

module Requests.CreateUser where

import Types.Json      (chop)
import Types.User      (UserId, RawPassword)

import Data.Aeson
import GHC.Generics    (Generic)

data CreateUser =
    CreateUser { cu_username    :: !UserId
               , cu_rawpassword :: !RawPassword
               } deriving Generic

instance FromJSON CreateUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
