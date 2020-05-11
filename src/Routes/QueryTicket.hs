{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryTicket where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Ticket

import Data.UUID                (UUID)

routeQueryTicket :: StorageApi
                 -> Maybe UUID      -- TODO type
                 -> Maybe UUID      -- TODO type
                 -> IO (Either ErrorResponse Ticket)
routeQueryTicket _         _    Nothing = errorResponse 400 "columnId not supplied."
routeQueryTicket _   Nothing          _ = errorResponse 400 "ticketId not supplied."
routeQueryTicket storageApi (Just cid) (Just tid) =
    Right <$> getTicket storageApi (ColumnId cid) (TicketId tid)