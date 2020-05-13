{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Routes.DeleteTicket where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import GHC.Generics               (Generic)

data DeleteTicket =
    DeleteTicket { dt_columnId :: !ColumnId
                 , dt_ticketId :: !TicketId
                 } deriving Generic

instance FromJSON DeleteTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeDeleteTicket :: StorageApi
                  -> DeleteTicket
                  -> ExceptT ErrorResponse IO ()
routeDeleteTicket storageApi (DeleteTicket columnId ticketId) =
    catchAll "Could not delete ticket."
             (lift $ deleteTicket storageApi columnId ticketId)
