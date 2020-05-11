{-# LANGUAGE DeriveGeneric #-}

module Routes.MoveTicket where

import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics           (Generic)
import Servant                (Handler)

data MoveTicket =
    MoveTicket { mt_board  :: !BoardName
               , mt_from   :: !ColumnId
               , mt_to     :: !ColumnId
               , mt_ticket :: !TicketId
               } deriving Generic

instance FromJSON MoveTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeMoveTicket :: StorageApi -> MoveTicket -> Handler ()
routeMoveTicket storageApi (MoveTicket board from to ticket) =
    liftIO . void $ moveTicket storageApi board from to ticket