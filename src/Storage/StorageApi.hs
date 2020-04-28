module Storage.StorageApi where

import Types.Board
import Types.Column
import Types.Ticket

import Data.Int  (Int32)
import Data.Text (Text)

data StorageApi = StorageApi
                { createBoardColumn :: BoardName -> Int32 -> ColumnName -> IO ColumnId
                , getBoard          :: BoardName -> IO Board
                , getDefaultColumn  :: BoardName -> IO (Maybe ColumnId)

                , createTicket      :: ColumnId -> Text -> Text -> IO TicketId
                , getColumn         :: ColumnId -> IO [Ticket]
                , moveTicket        :: BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
                }