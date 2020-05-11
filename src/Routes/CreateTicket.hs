{-# LANGUAGE DeriveGeneric
           , LambdaCase
           , OverloadedStrings #-}

module Routes.CreateTicket where

import Routes.Shared
import Storage.StorageApi
import Types.Board
import Types.Json
import Types.Ticket

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics           (Generic)
import Servant                (Handler)

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
                  -> Handler TicketId
routeCreateTicket storageApi (CreateTicket boardName name body) =
    (liftIO $ getDefaultColumn storageApi boardName) >>= \case
        Just cid -> liftIO $ createTicket storageApi cid name body
        Nothing  -> err 500 "No default column found."