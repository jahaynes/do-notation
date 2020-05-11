{-# LANGUAGE DeriveGeneric #-}

module Requests.UpdateTicket where

import Types.Column
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics (Generic)

data UpdateTicket =
    UpdateTicket { ut_columnId :: !ColumnId
                 , ut_ticketId :: !TicketId
                 , ut_name     :: !TicketName
                 , ut_content  :: !TicketContent
                 } deriving Generic

instance FromJSON UpdateTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
