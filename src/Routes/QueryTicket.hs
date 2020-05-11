{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryTicket where

import Routes.Shared
import Storage.StorageApi
import Types.Column
import Types.Ticket

import Control.Monad.IO.Class   (liftIO)
import Data.UUID                (UUID)
import Servant

routeQueryTicket :: StorageApi
                 -> Maybe UUID      -- TODO type
                 -> Maybe UUID      -- TODO type
                 -> Handler Ticket
routeQueryTicket _         _    Nothing = err 400 "columnId not supplied."
routeQueryTicket _   Nothing          _ = err 400 "ticketId not supplied."
routeQueryTicket storageApi (Just cid) (Just tid) =
    liftIO $ getTicket storageApi (ColumnId cid) (TicketId tid)