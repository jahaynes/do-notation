{-# LANGUAGE DeriveGeneric #-}

module Requests.Login where

import Types.Json      (chop)
import Types.User      (UserId, RawPassword)

import Data.Aeson
import GHC.Generics    (Generic)

data Login =
    Login { l_username    :: !UserId
          , l_rawpassword :: !RawPassword
          } deriving Generic

instance FromJSON Login where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
