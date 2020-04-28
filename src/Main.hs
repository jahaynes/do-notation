{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller
import Storage.StorageApi
import Storage.Cassandra.Connection
import Storage.Cassandra.Queries
import Storage.Cassandra.Keyspace
import Storage.Cassandra.Tables
import Types.Board
import Types.Column

import Safe               (readEitherSafe)
import System.Environment (lookupEnv)

main :: IO ()
main = do

    cassandraPort  <- parseEnv "CASS_PORT"

    cassandraHosts <- parseEnv "CASS_HOSTS"

    storageApi <- do 
        c <- create cassandraPort cassandraHosts
        createKeyspace c 2
        createTables c
        pure $ createApi c

    let boardName = BoardName "some-board"
    _   <- createBoardColumn storageApi boardName 1 (ColumnName "wish-list")
    ci2 <- createBoardColumn storageApi boardName 2 (ColumnName "in-progress")
    _   <- createBoardColumn storageApi boardName 3 (ColumnName "done")
    _   <- createTicket storageApi ci2 "ticket name 1" "ticket content 1"
    _   <- createTicket storageApi ci2 "ticket name 2" "ticket content 2"
    _   <- createTicket storageApi ci2 "ticket name 3" "ticket content 3"

    runServer storageApi
    pure ()

parseEnv :: Read a => String -> IO a
parseEnv key = get . env <$> lookupEnv key
    where
    get (Left e)  = error e
    get (Right r) = r
    env var = maybe (Left $ "Env var not found: " <> key) Right var >>= readEitherSafe
