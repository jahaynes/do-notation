{-# LANGUAGE DeriveGeneric
           , LambdaCase
           , OverloadedStrings #-}

module Routes.CreateTicket where

import Errors
import Storage.StorageApi
import Types.Board
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics           (Generic)

data CreateTicket =
    CreateTicket { ct_board   :: !BoardName
                 , ct_name    :: !TicketName
                 , ct_content :: !TicketContent
                 } deriving Generic

instance FromJSON CreateTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeCreateTicket :: StorageApi
                  -> CreateTicket
                  -> IO (Either ErrorResponse TicketId)
routeCreateTicket storageApi (CreateTicket boardName name body) =
    getDefaultColumn storageApi boardName >>= \case
        Just cid -> Right <$> createTicket storageApi cid name body
        Nothing  -> errorResponse 500 "No default column found."