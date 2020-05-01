module Storage.StorageApi where

import Types.Board
import Types.Column
import Types.Ticket

import Data.Text (Text)

data StorageApi = StorageApi
                { createBoardColumn :: BoardName -> Int -> ColumnName -> IO ColumnId
                , getBoard          :: BoardName -> IO Board
                , getDefaultColumn  :: BoardName -> IO (Maybe ColumnId)

                , createTicket      :: ColumnId -> Text -> Text -> IO TicketId
                , deleteTicket      :: ColumnId -> TicketId -> IO ()
                , getTicket         :: ColumnId -> TicketId -> IO Ticket
                , getColumn         :: ColumnId -> IO [Ticket]
                , moveTicket        :: BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
                }