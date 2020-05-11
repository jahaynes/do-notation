{-# LANGUAGE DeriveGeneric #-}

module Routes.UpdateTicket where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics           (Generic)

data UpdateTicket =
    UpdateTicket { ut_columnId :: !ColumnId
                 , ut_ticketId :: !TicketId
                 , ut_name     :: !TicketName
                 , ut_content  :: !TicketContent
                 } deriving Generic

instance FromJSON UpdateTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeUpdateTicket :: StorageApi
                  -> UpdateTicket
                  -> IO (Either ErrorResponse ())
routeUpdateTicket storageApi (UpdateTicket colId ticketId name body) =
    Right <$> updateTicket storageApi colId ticketId name body
