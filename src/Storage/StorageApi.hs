module Storage.StorageApi where

import Types.Board
import Types.Column
import Types.Ticket

data StorageApi = StorageApi
                { createBoardColumn :: BoardName -> ColumnPosition -> ColumnName -> IO ColumnId
                , getBoard          :: BoardName -> IO Board
                , getDefaultColumn  :: BoardName -> IO (Maybe ColumnId)

                , createTicket      :: ColumnId -> TicketName -> TicketContent -> IO TicketId
                , deleteTicket      :: ColumnId -> TicketId -> IO ()
                , getTicket         :: ColumnId -> TicketId -> IO Ticket
                , getColumn         :: ColumnId -> IO [Ticket]
                , moveTicket        :: BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
                }