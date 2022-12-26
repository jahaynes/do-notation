{-# LANGUAGE OverloadedStrings #-}

module Storage.Cassandra.Keyspace (createKeyspace) where

import Storage.Cassandra.Common (params)

import Control.Monad   (void)
import Data.Text.Lazy  (Text, pack)
import Database.CQL.IO

createKeyspace :: ClientState -> Int -> IO ()
createKeyspace c replication = void 
                             . runClient c
                             . schema (QueryString cql)
                             $ params ()
    where
    cql :: Text
    cql = " CREATE KEYSPACE IF NOT EXISTS            \
          \ do_notation                              \
          \ WITH replication =                       \
          \    { 'class':'SimpleStrategy'            \
          \    , 'replication_factor':" <> rf <> "}; "

        where
        rf = pack . show $ replication