{-# LANGUAGE DeriveGeneric #-}

module Routes.DeleteTicket where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics           (Generic)

data DeleteTicket =
    DeleteTicket { dt_columnId :: !ColumnId
                 , dt_ticketId :: !TicketId
                 } deriving Generic

instance FromJSON DeleteTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeDeleteTicket :: StorageApi
                  -> DeleteTicket
                  -> IO (Either ErrorResponse ())
routeDeleteTicket storageApi (DeleteTicket columnId ticketId) =
    Right <$> deleteTicket storageApi columnId ticketId
