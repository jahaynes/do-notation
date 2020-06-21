{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller
import Routes.Health as H
import Security.Security
import Storage.Cassandra.Connection as C
import Storage.Cassandra.Queries
import Storage.Cassandra.Keyspace
import Storage.Cassandra.Tables
import Storage.Sqlite
import Storage.StorageApi

import Safe               (readEitherSafe)
import System.Environment (lookupEnv)

data Implementation = Cassandra
                    | Sqlite

getSecurityApi :: IO (SecurityApi IO)
getSecurityApi = parseEnv "JWT_SECRET" >>= createSecurityApi

getPort :: IO Int
getPort = parseEnv "PORT"

getStorageApi :: Implementation -> IO StorageApi
getStorageApi Sqlite = createSqlite "data/do-notation.db"
getStorageApi Cassandra = do
    cassandraPort  <- parseEnv "CASS_PORT"
    cassandraHosts <- parseEnv "CASS_HOSTS"
    C.create cassandraPort cassandraHosts >>= \c -> do
        createKeyspace c 2
        createTables c
        pure $ createApi c

main :: IO ()
main = do
    healthApi   <- H.create
    port        <- getPort
    securityApi <- getSecurityApi
    storageApi  <- getStorageApi Sqlite
    runServer healthApi port securityApi storageApi

parseEnv :: Read a => String -> IO a
parseEnv key = get . env <$> lookupEnv key
    where
    get (Left e)  = error e
    get (Right r) = r
    env var = maybe (Left $ "Env var not found: " <> key) Right var >>= readEitherSafe
