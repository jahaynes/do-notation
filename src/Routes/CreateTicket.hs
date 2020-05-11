{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module Routes.CreateTicket where

import Requests.CreateTicket
import Routes.Shared
import Storage.StorageApi
import Types.Ticket

import Control.Monad.IO.Class (liftIO)
import Servant                (Handler)

routeCreateTicket :: StorageApi
                  -> CreateTicket
                  -> Handler TicketId
routeCreateTicket storageApi (CreateTicket boardName name body) =
    (liftIO $ getDefaultColumn storageApi boardName) >>= \case
        Just cid -> liftIO $ createTicket storageApi cid name body
        Nothing  -> err 500 "No default column found."