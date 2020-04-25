{-# LANGUAGE OverloadedStrings #-}

module Storage.Cassandra where

import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Ticket

import Prelude hiding (init)

import           Control.Monad               (forM, void)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Int                    (Int32)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy        as TL

import           Data.Vector                 (Vector)
import qualified Data.Vector           as V
import           Data.Vector.Algorithms.Heap (sort)

import           Network.Socket              (PortNumber)
import           Data.UUID                   (UUID)
import qualified Data.UUID.V4          as U
import           Database.CQL.Protocol
import           Database.CQL.IO 

createKeyspace :: TL.Text -> Int -> Client ()
createKeyspace ks replication = void $ schema (QueryString cql) (params ())
    where
    cql :: TL.Text
    cql = TL.unwords [ "CREATE KEYSPACE IF NOT EXISTS"
                     , ks
                     , "WITH replication ="
                     , "{'class':'SimpleStrategy', 'replication_factor':" <> rf <> "};"
                     ]
        where
        rf = TL.pack . show $ replication

params :: a -> QueryParams a
params p = QueryParams
    { consistency       = One
    , skipMetaData      = False
    , values            = p
    , pageSize          = Nothing
    , queryPagingState  = Nothing
    , serialConsistency = Nothing
    , enableTracing     = Nothing
    }

getBoardImpl :: ClientState -> BoardId -> IO Board
getBoardImpl c (BoardId boardId) =
    runClient c $ do
        result <- query cqlGetBoard (defQueryParams One (Identity boardId))
        pure . Board
             . V.map (\(_, cn, ci) -> Column (ColumnName cn) (ColumnId ci))
             . V.modify sort
             . V.fromList
             $ result
    where
    cqlGetBoard :: PrepQuery R (Identity Text) (Int32, Text, UUID)
    cqlGetBoard =
        " SELECT position, columnname, columnid \
        \ FROM do_notation.board                \
        \ WHERE name = ?                        "

getDefaultColumnImpl :: ClientState -> BoardId -> IO (Maybe ColumnId)
getDefaultColumnImpl c (BoardId boardId) =
    runClient c $ do
        mRow <- query1 cqlGetDefaultColumn (params (Identity boardId))
        pure $ case mRow of
            Nothing             -> Nothing
            Just (Identity row) -> Just (ColumnId row)

    where
    cqlGetDefaultColumn :: PrepQuery R (Identity Text) (Identity UUID)
    cqlGetDefaultColumn =
        " SELECT columnid        \
        \ FROM do_notation.board \
        \ WHERE name = ?         "

createBoardColumnImpl :: ClientState -> BoardId -> Int32 -> ColumnName -> IO ColumnId
createBoardColumnImpl c (BoardId boardId) position (ColumnName name) = do
    freshCid <- U.nextRandom
    runClient c $ write cqlCreateBoardColumn (params (boardId, position, name, freshCid))
    pure $ ColumnId freshCid

    where
    cqlCreateBoardColumn :: PrepQuery W (Text, Int32, Text, UUID) ()
    cqlCreateBoardColumn =
        " INSERT INTO do_notation.board                 \
        \ (name, position, columnname, columnid) VALUES \
        \ (   ?,        ?,          ?,        ?)        "

createTicketImpl :: ClientState -> ColumnId -> Text -> Text -> IO TicketId
createTicketImpl c (ColumnId cid) name content = do
    freshTid <- U.nextRandom
    runClient c $ write cqlCreateTicket (params (cid, freshTid, name, content))
    pure $ TicketId freshTid

    where
    cqlCreateTicket :: PrepQuery W (UUID, UUID, Text, Text) ()
    cqlCreateTicket =
        " INSERT INTO do_notation.ticket       \
        \ (columnid, id, name, content) VALUES \
        \ (       ?,  ?,    ?,       ?)        "

getColumnImpl :: ClientState -> ColumnId -> IO [Ticket]
getColumnImpl c (ColumnId cid) =
    runClient c $ map toTicket <$> query cqlGetColumn (params (Identity cid))
    where
    toTicket (ti, name, content) = Ticket (TicketId ti) name content

    cqlGetColumn :: PrepQuery R (Identity UUID) (UUID, Text, Text)
    cqlGetColumn =
        " SELECT id, content, name \
        \ FROM do_notation.ticket  \
        \ WHERE columnId = ?       "

create :: PortNumber -> [String] -> IO StorageApi
create    _       [] = error "FATAL.  No Cassandra hosts supplied"
create port (h:osts) = do

    c <- init . setPortNumber port 
              . setContacts h osts
              $ defSettings

    runClient c $ do
        createKeyspace "do_notation" 1
        _ <- schema (QueryString  schemaCreateBoard) (params ())
        _ <- schema (QueryString schemaCreateTicket) (params ())
        pure ()

    pure $ StorageApi { createBoardColumn = createBoardColumnImpl c
                      , getBoard          = getBoardImpl c
                      , getDefaultColumn  = getDefaultColumnImpl c

                      , getColumn         = getColumnImpl c
                      , createTicket      = createTicketImpl c
                      }

    where
    schemaCreateBoard :: TL.Text
    schemaCreateBoard = 
        " CREATE TABLE IF NOT EXISTS                     \
        \   do_notation.board                            \
        \     ( name       TEXT                          \
        \     , position   INT                           \
        \     , columnName TEXT                          \
        \     , columnId   UUID                          \
        \     , PRIMARY KEY (name, position, columnName) \
        \     ) WITH CLUSTERING ORDER BY (position ASC, columnName ASC) "

    schemaCreateTicket :: TL.Text
    schemaCreateTicket = 
        " CREATE TABLE IF NOT EXISTS       \
        \   do_notation.ticket             \
        \     ( columnId UUID              \
        \     , id       UUID              \
        \     , name     TEXT              \
        \     , content  TEXT              \
        \     , PRIMARY KEY (columnId, id) \
        \     )                            "