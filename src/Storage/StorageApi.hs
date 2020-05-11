module Storage.StorageApi where

import Types.Board
import Types.Column
import Types.Ticket
import Types.User

data StorageApi = StorageApi
                { createUser         :: UserId -> Salt -> HashedSaltedPassword -> IO ()
                , getSaltAndPassword :: UserId -> IO (Maybe (Salt, HashedSaltedPassword))
                , createBoardColumn  :: BoardName -> ColumnPosition -> ColumnName -> IO ColumnId
                , getBoard           :: BoardName -> IO Board
                , getDefaultColumn   :: BoardName -> IO (Maybe ColumnId)
 
                , createTicket       :: ColumnId -> TicketName -> TicketContent -> IO TicketId
                , updateTicket       :: ColumnId -> TicketId -> TicketName -> TicketContent -> IO ()
                , deleteTicket       :: ColumnId -> TicketId -> IO ()
                , getTicket          :: ColumnId -> TicketId -> IO Ticket
                , getColumn          :: ColumnId -> IO [Ticket]
                , moveTicket         :: BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
                }
