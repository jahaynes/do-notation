module Routes.DeleteTicket where

import Requests.DeleteTicket
import Storage.StorageApi

import Control.Monad.IO.Class (liftIO)
import Servant                (Handler)

routeDeleteTicket :: StorageApi -> DeleteTicket -> Handler ()
routeDeleteTicket storageApi (DeleteTicket columnId ticketId) =
    liftIO $ deleteTicket storageApi columnId ticketId