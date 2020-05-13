{-# LANGUAGE DeriveGeneric
           , LambdaCase
           , OverloadedStrings #-}

module Routes.CreateTicket where

import Errors
import Storage.StorageApi
import Types.Board
import Types.Json
import Types.Ticket

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import GHC.Generics               (Generic)

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
                  -> ExceptT ErrorResponse IO TicketId
routeCreateTicket storageApi (CreateTicket boardName name body) =
    catchAll "Could not create ticket." $
        lift (getDefaultColumn storageApi boardName) >>= \case
            Just defaultColumnId -> lift (createTicket storageApi defaultColumnId name body)
            Nothing              -> err' 500 "No default column found."