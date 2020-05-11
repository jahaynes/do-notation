{-# LANGUAGE OverloadedStrings #-}

module Storage.Cassandra.Tables where

import Storage.Cassandra.Common (params)

import Control.Monad   (void)
import Data.Text.Lazy  (Text)
import Database.CQL.IO 

createTables :: ClientState -> IO ()
createTables c =
    runClient c $ do
        void $ schema (QueryString   schemaCreateUser) (params ())
        void $ schema (QueryString  schemaCreateBoard) (params ())
        void $ schema (QueryString schemaCreateTicket) (params ())

    where
    schemaCreateUser :: Text
    schemaCreateUser =
        " CREATE TABLE IF NOT EXISTS  \
        \   do_notation.user          \
        \     ( userid           TEXT \
        \     , salt             BLOB \
        \     , hashsaltpassword BLOB \
        \     , PRIMARY KEY (userid)  \ 
        \     )                       "

    schemaCreateBoard :: Text
    schemaCreateBoard = 
        " CREATE TABLE IF NOT EXISTS                     \
        \   do_notation.board                            \
        \     ( name       TEXT                          \
        \     , position   INT                           \
        \     , columnName TEXT                          \
        \     , columnId   UUID                          \
        \     , PRIMARY KEY (name, position, columnName) \
        \     ) WITH CLUSTERING ORDER BY (position ASC, columnName ASC) "

    schemaCreateTicket :: Text
    schemaCreateTicket = 
        " CREATE TABLE IF NOT EXISTS       \
        \   do_notation.ticket             \
        \     ( columnId UUID              \
        \     , id       UUID              \
        \     , name     TEXT              \
        \     , content  TEXT              \
        \     , PRIMARY KEY (columnId, id) \
        \     )                            "
