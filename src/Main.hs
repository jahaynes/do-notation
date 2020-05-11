{-# LANGUAGE OverloadedStrings #-}

module Main where


import Controller
import Security.Security
import Storage.Cassandra.Connection
import Storage.Cassandra.Queries
import Storage.Cassandra.Keyspace
import Storage.Cassandra.Tables
import Storage.Sqlite
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Ticket

import Control.Monad      (when)
import Safe               (readEitherSafe)
import System.Environment (getArgs, lookupEnv)

data Implementation = Sqlite
                    | Cassandra

getApi :: Implementation -> IO StorageApi
getApi Sqlite = createSqlite "data/do-notation.db"
getApi Cassandra = do
    cassandraPort  <- parseEnv "CASS_PORT"
    cassandraHosts <- parseEnv "CASS_HOSTS"
    create cassandraPort cassandraHosts >>= \c -> do
        createKeyspace c 2
        createTables c
        pure $ createApi c

main :: IO ()
main = do

    securityApi <- createSecurityApi
    storageApi <- getApi Sqlite
    args <- getArgs
    when (args == ["--sample-data"]) $ do
        putStrLn "Generating sample data"
        sampleData storageApi
    runServer securityApi storageApi

sampleData :: StorageApi -> IO ()
sampleData storageApi = do
    let boardName = BoardName "some-board"
    _   <- createBoardColumn storageApi boardName (ColumnPosition 1) (ColumnName "wish-list")
    ci2 <- createBoardColumn storageApi boardName (ColumnPosition 2) (ColumnName "in-progress")
    _   <- createBoardColumn storageApi boardName (ColumnPosition 3) (ColumnName "done")
    _   <- createTicket storageApi ci2 (TicketName "ticket name 1") (TicketContent "ticket content 1")
    _   <- createTicket storageApi ci2 (TicketName "ticket name 2") (TicketContent "ticket content 2")
    _   <- createTicket storageApi ci2 (TicketName "ticket name 3") (TicketContent "ticket content 3")
    pure ()

parseEnv :: Read a => String -> IO a
parseEnv key = get . env <$> lookupEnv key
    where
    get (Left e)  = error e
    get (Right r) = r
    env var = maybe (Left $ "Env var not found: " <> key) Right var >>= readEitherSafe
