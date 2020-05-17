{-# LANGUAGE OverloadedStrings #-}

module Storage.Cassandra.Tables where

import Storage.Cassandra.Common (params)

import Control.Monad   (void)
import Data.Text.Lazy  (Text)
import Database.CQL.IO 

createTables :: ClientState -> IO ()
createTables c =
    runClient c $ do
        void $ schema (QueryString schemaCreatePassword) (params ())
        void $ schema (QueryString     schemaCreateUser) (params ())
        void $ schema (QueryString    schemaCreateBoard) (params ())
        void $ schema (QueryString   schemaCreateColumn) (params ())
        void $ schema (QueryString   schemaCreateTicket) (params ())

    where
    schemaCreatePassword :: Text
    schemaCreatePassword =
        " CREATE TABLE IF NOT EXISTS  \
        \   do_notation.password      \
        \     ( userid           TEXT \
        \     , salt             BLOB \
        \     , hashsaltpassword BLOB \
        \     , PRIMARY KEY (userid)  \ 
        \     )                       "

    -- Get all boards for userId UI.
    schemaCreateUser :: Text
    schemaCreateUser =
        " CREATE TABLE IF NOT EXISTS          \
        \   do_notation.user                  \
        \     ( userid  TEXT                  \
        \     , boardid UUID                  \
        \     , PRIMARY KEY (userid, boardid) \
        \     )                               "

    schemaCreateBoard :: Text
    schemaCreateBoard = 
        " CREATE TABLE IF NOT EXISTS             \
        \   do_notation.board                    \
        \     ( boardid   UUID                   \
        \     , boardname TEXT                   \
        \     , PRIMARY KEY (boardid, boardname) \
        \     )                                  "

    -- Get all columns for boardId BI.
    schemaCreateColumn :: Text
    schemaCreateColumn =
        " CREATE TABLE IF NOT EXISTS            \
        \   do_notation.column                  \
        \     ( boardid     UUID                \
        \     , columnid    UUID                \
        \     , position    INT                 \
        \     , columnname  TEXT                \
        \     , PRIMARY KEY (boardid, columnid) \
        \     )                                 "

    -- Get all tickets for columnId CI.
    schemaCreateTicket :: Text
    schemaCreateTicket =
        " CREATE TABLE IF NOT EXISTS       \
        \   do_notation.ticket             \
        \     ( columnid UUID              \
        \     , id       UUID              \
        \     , name     TEXT              \
        \     , content  TEXT              \
        \     , PRIMARY KEY (columnid, id) \
        \     )                            "
