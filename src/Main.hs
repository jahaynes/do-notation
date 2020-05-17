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
import Types.User

import Control.Monad      (when)
import Safe               (readEitherSafe)
import System.Environment (getArgs, lookupEnv)

data Implementation = Cassandra
                    | Sqlite

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
    b1  <- createBoard storageApi (BoardName "some-board")
    b2  <- createBoard storageApi (BoardName "board 2")
    _   <- createUser storageApi (UserId "foo") b1
    _   <- createUser storageApi (UserId "foo") b2

    ci1 <- createColumn storageApi b1 (ColumnPosition 1) (ColumnName "Wish List")
    ci2 <- createColumn storageApi b1 (ColumnPosition 2) (ColumnName "In Progress")
    ci3 <- createColumn storageApi b1 (ColumnPosition 3) (ColumnName "Done")

    _ <- createTicket storageApi ci2 (TicketName "ticket name 1") (TicketContent "ticket content 1")
    _ <- createTicket storageApi ci2 (TicketName "ticket name 2") (TicketContent "ticket content 2")
    _ <- createTicket storageApi ci2 (TicketName "ticket name 3") (TicketContent "ticket content 3")
    pure ()

parseEnv :: Read a => String -> IO a
parseEnv key = get . env <$> lookupEnv key
    where
    get (Left e)  = error e
    get (Right r) = r
    env var = maybe (Left $ "Env var not found: " <> key) Right var >>= readEitherSafe
