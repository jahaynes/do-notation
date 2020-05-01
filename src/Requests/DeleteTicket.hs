{-# LANGUAGE DeriveGeneric #-}

module Requests.DeleteTicket where

import Types.Column
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics    (Generic)

data DeleteTicket =
    DeleteTicket { dt_columnId :: !ColumnId
                 , dt_ticketId :: !TicketId
                 } deriving Generic

instance FromJSON DeleteTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
