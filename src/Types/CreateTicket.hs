{-# LANGUAGE DeriveGeneric #-}

module Types.CreateTicket where

import Types.Board
import Types.Json

import Data.Aeson
import Data.Text       (Text)
import GHC.Generics    (Generic)

data CreateTicket =
    CreateTicket { ct_board   :: !BoardId
                 , ct_name    :: !Text
                 , ct_content :: !Text
                 } deriving Generic

instance FromJSON CreateTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
