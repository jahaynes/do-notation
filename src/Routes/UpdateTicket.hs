module Routes.UpdateTicket where

import Requests.UpdateTicket
import Storage.StorageApi

import Control.Monad.IO.Class (liftIO)
import Servant                (Handler)

routeUpdateTicket :: StorageApi
                  -> UpdateTicket
                  -> Handler ()
routeUpdateTicket storageApi (UpdateTicket colId ticketId name body) =
    liftIO $ updateTicket storageApi colId ticketId name body
