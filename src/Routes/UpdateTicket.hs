{-# LANGUAGE DeriveGeneric #-}

module Routes.UpdateTicket where

import Storage.StorageApi
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics           (Generic)
import Servant                (Handler)

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
                  -> Handler ()
routeUpdateTicket storageApi (UpdateTicket colId ticketId name body) =
    liftIO $ updateTicket storageApi colId ticketId name body
