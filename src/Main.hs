{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller
import Storage.StorageApi
import Storage.Cassandra
import Types.Board
import Types.Column

import Data.UUID
import Safe               (readEitherSafe)
import System.Environment (lookupEnv)

main :: IO ()
main = do

    cassandraPort  <- parseEnv "CASS_PORT"
    cassandraHosts <- parseEnv "CASS_HOSTS"

    storageApi <- create cassandraPort cassandraHosts

    let boardId = BoardId "some-board"
    ci1 <- createBoardColumn storageApi boardId 1 (ColumnName "wish-list")
    ci2 <- createBoardColumn storageApi boardId 2 (ColumnName "in-progress")
    ci3 <- createBoardColumn storageApi boardId 3 (ColumnName "done")
    print =<< getBoard storageApi boardId
    print =<< getDefaultColumn storageApi boardId
    ti1 <- createTicket storageApi ci2 "ticket name 1" "ticket content 1"
    ti2 <- createTicket storageApi ci2 "ticket name 2" "ticket content 2"
    ti3 <- createTicket storageApi ci2 "ticket name 3" "ticket content 3"
    print =<< getColumn storageApi ci2

    runServer storageApi

parseEnv :: Read a => String -> IO a
parseEnv key = get . env <$> lookupEnv key
    where
    get (Left e)  = error e
    get (Right r) = r
    env var = maybe (Left $ "Env var not found: " <> key) Right var >>= readEitherSafe
