{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Routes.UpdateTicket where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import GHC.Generics               (Generic)

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
                  -> ExceptT ErrorResponse IO ()
routeUpdateTicket storageApi (UpdateTicket colId ticketId name body) =
    catchAll "Could not update ticket."
              (lift $ updateTicket storageApi colId ticketId name body)
