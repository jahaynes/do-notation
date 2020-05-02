{-# LANGUAGE DeriveGeneric #-}

module Requests.CreateTicket where

import Types.Board
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics (Generic)

data CreateTicket =
    CreateTicket { ct_board   :: !BoardName
                 , ct_name    :: !TicketName
                 , ct_content :: !TicketContent
                 } deriving Generic

instance FromJSON CreateTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
