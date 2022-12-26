{-# LANGUAGE DeriveGeneric,
             OverloadedStrings #-}

module Routes.MoveTicket (MoveTicket, routeMoveTicket) where

import Errors
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad              (void)
import Data.Aeson
import GHC.Generics               (Generic)

data MoveTicket =
    MoveTicket { mt_board  :: !BoardName
               , mt_from   :: !ColumnId
               , mt_to     :: !ColumnId
               , mt_ticket :: !TicketId
               } deriving Generic

instance FromJSON MoveTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeMoveTicket :: StorageApi
                -> MoveTicket
                -> ExceptT ErrorResponse IO ()
routeMoveTicket storageApi (MoveTicket board from to ticket) =
    catchAll "Could not move ticket"
             (lift . void $ moveTicket storageApi board from to ticket)
