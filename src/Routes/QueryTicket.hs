{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryTicket where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Ticket

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.UUID                  (UUID)

routeQueryTicket :: StorageApi
                 -> Maybe UUID      -- TODO type
                 -> Maybe UUID      -- TODO type
                 -> ExceptT ErrorResponse IO Ticket
routeQueryTicket storageApi mColumnId mTicketId = do

    columnId <- getColumnId mColumnId

    ticketId <- getTicketId mTicketId

    catchAll "Could not query ticket"
             (lift $ getTicket storageApi (ColumnId columnId) (TicketId ticketId))

    where
    getColumnId Nothing    = err' 400 "columnId not supplied."
    getColumnId (Just cid) = pure cid

    getTicketId Nothing    = err' 400 "ticketId not supplied."
    getTicketId (Just tid) = pure tid
