{-# LANGUAGE DeriveGeneric #-}

module Types.MoveTicket where

import Types.Board
import Types.Column
import Types.Json
import Types.Ticket

import Data.Aeson
import GHC.Generics    (Generic)

data MoveTicket =
    MoveTicket { mt_board  :: !BoardId
               , mt_from   :: !ColumnId
               , mt_to     :: !ColumnId
               , mt_ticket :: !TicketId
               } deriving Generic

instance FromJSON MoveTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
