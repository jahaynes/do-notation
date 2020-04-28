{-# LANGUAGE OverloadedStrings #-}

module Storage.Cassandra.Tables where

import Storage.Cassandra.Common (params)

import           Control.Monad         (void)
import qualified Data.Text.Lazy  as TL
import           Database.CQL.IO 

createTables :: ClientState -> IO ()
createTables c =
    runClient c $ do
        void $ schema (QueryString  schemaCreateBoard) (params ())
        void $ schema (QueryString schemaCreateTicket) (params ())

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
