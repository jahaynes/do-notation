{-# LANGUAGE DeriveGeneric #-}

module Routes.DeleteTicket where

import Storage.StorageApi
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics           (Generic)
import Servant                (Handler)

data DeleteTicket =
    DeleteTicket { dt_columnId :: !ColumnId
                 , dt_ticketId :: !TicketId
                 } deriving Generic

instance FromJSON DeleteTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeDeleteTicket :: StorageApi -> DeleteTicket -> Handler ()
routeDeleteTicket storageApi (DeleteTicket columnId ticketId) =
    liftIO $ deleteTicket storageApi columnId ticketId
