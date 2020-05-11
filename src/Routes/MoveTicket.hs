{-# LANGUAGE DeriveGeneric #-}

module Routes.MoveTicket where

import Errors
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Json
import Types.Ticket

import Control.Monad          (void)
import Data.Aeson
import GHC.Generics           (Generic)

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
                -> IO (Either ErrorResponse ())
routeMoveTicket storageApi (MoveTicket board from to ticket) =
    Right <$> (void $ moveTicket storageApi board from to ticket)