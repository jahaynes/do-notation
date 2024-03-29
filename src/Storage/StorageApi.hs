module Storage.StorageApi (StorageApi (..)) where

import Errors
import Types.Board
import Types.BoardId
import Types.Column
import Types.Ticket
import Types.User

data StorageApi = StorageApi
                { createPassword     :: UserId -> Salt -> HashedSaltedPassword -> IO (Either ErrorResponse ())
                , getSaltAndPassword :: UserId -> IO (Maybe (Salt, HashedSaltedPassword))

                , createUser         :: UserId -> BoardId -> IO ()
                , createBoard        :: BoardName -> IO BoardId
                , deleteBoard        :: BoardId -> IO ()
                , createColumn       :: BoardId -> ColumnPosition -> ColumnName -> IO ColumnId

                , getBoard           :: BoardId -> IO (Maybe Board)
                , getBoards          :: UserId -> IO [(BoardId, BoardName)]
                , getDefaultColumn   :: BoardId -> IO (Maybe ColumnId)
 
                , createTicket       :: ColumnId -> TicketName -> TicketContent -> IO TicketId
                , updateTicket       :: ColumnId -> TicketId -> TicketName -> TicketContent -> IO ()
                , deleteTicket       :: ColumnId -> TicketId -> IO ()
                , getTicket          :: ColumnId -> TicketId -> IO (Maybe Ticket)
                , getColumnTickets   :: ColumnId -> IO [Ticket]

                -- TODO boardname/boardid?
                , moveTicket         :: BoardName -> ColumnId -> ColumnId -> TicketId -> IO (Either ErrorResponse ()) 
                }
