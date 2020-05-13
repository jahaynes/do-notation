{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module Storage.Cassandra.Queries where

import Storage.Cassandra.Common (params)
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Ticket
import Types.User

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int                     (Int32)
import           Data.Text                    (Text)
import qualified Data.Vector           as V
import           Data.Vector.Algorithms.Heap  (sort)
import           Data.UUID                    (UUID)
import qualified Data.UUID.V4          as U
import           Database.CQL.IO 
import           Database.CQL.Protocol        (Blob (Blob))

createApi :: ClientState -> StorageApi
createApi c = 
    StorageApi { createUser         = createUserImpl c
               , getSaltAndPassword = getSaltAndPasswordImpl c
               , createBoardColumn  = createBoardColumnImpl c
               , getBoard           = getBoardImpl c
               , getDefaultColumn   = getDefaultColumnImpl c
               , getColumn          = getColumnImpl c
               , createTicket       = createTicketImpl c
               , updateTicket       = updateTicketImpl c
               , deleteTicket       = deleteTicketImpl c
               , getTicket          = getTicketImpl c
               , moveTicket         = moveTicketImpl c
               }

blob :: ByteString -> Blob
blob = Blob . LBS.fromStrict

createUserImpl :: ClientState -> UserId -> Salt -> HashedSaltedPassword -> IO ()
createUserImpl c (UserId userId) (Salt salt) (HashedSaltedPassword hspw) =
    runClient c $ write cqlCreateUser (params (userId, blob salt, blob hspw))
    where
    cqlCreateUser :: PrepQuery W (Text, Blob, Blob) ()
    cqlCreateUser =
        " INSERT INTO do_notation.user            \
        \ (userid, salt, hashsaltpassword) VALUES \
        \ (     ?,    ?,                ?)        "

getSaltAndPasswordImpl :: ClientState -> UserId -> IO (Maybe (Salt, HashedSaltedPassword))
getSaltAndPasswordImpl c (UserId userid) =
    handle <$> (runClient c $ query cqlGetSaltAndPassword (defQueryParams One (Identity userid)))
    where
    handle                       [] = Nothing
    handle [(Blob salt, Blob hspw)] = Just (Salt $ LBS.toStrict salt, HashedSaltedPassword $ LBS.toStrict hspw)
    handle                        _ = error "Too many userid"

    cqlGetSaltAndPassword :: PrepQuery R (Identity Text) (Blob, Blob)
    cqlGetSaltAndPassword =
        " SELECT salt, hashsaltpassword \
        \ FROM do_notation.user         \
        \ WHERE userid = ?              "

getBoardImpl :: ClientState -> BoardName -> IO Board
getBoardImpl c (BoardName boardName) = runClient c $
    asBoard <$> query cqlGetBoard (defQueryParams One (Identity boardName))
    where
    asBoard :: [(Int32, Text, UUID)] -> Board
    asBoard = Board
            . V.map (\(_, cn, ci) -> Column (ColumnName cn) (ColumnId ci))
            . V.modify sort
            . V.fromList

    cqlGetBoard :: PrepQuery R (Identity Text) (Int32, Text, UUID)
    cqlGetBoard =
        " SELECT position, columnname, columnid \
        \ FROM do_notation.board                \
        \ WHERE name = ?                        "

getDefaultColumnImpl :: ClientState -> BoardName -> IO (Maybe ColumnId)
getDefaultColumnImpl c (BoardName boardName) =
    runClient c $
        query1 cqlGetDefaultColumn (params (Identity boardName)) >>= \case
            Nothing             -> pure Nothing
            Just (Identity row) -> pure $ Just (ColumnId row)

    where
    cqlGetDefaultColumn :: PrepQuery R (Identity Text) (Identity UUID)
    cqlGetDefaultColumn =
        " SELECT columnid        \
        \ FROM do_notation.board \
        \ WHERE name = ?         "

createBoardColumnImpl :: ClientState -> BoardName -> ColumnPosition -> ColumnName -> IO ColumnId
createBoardColumnImpl c (BoardName boardName) (ColumnPosition pos) (ColumnName name) = do
    freshCid <- U.nextRandom
    runClient c $ write cqlCreateBoardColumn (params (boardName, fromIntegral pos, name, freshCid))
    pure $ ColumnId freshCid

    where
    cqlCreateBoardColumn :: PrepQuery W (Text, Int32, Text, UUID) ()
    cqlCreateBoardColumn =
        " INSERT INTO do_notation.board                 \
        \ (name, position, columnname, columnid) VALUES \
        \ (   ?,        ?,          ?,        ?)        "

createTicketImpl :: ClientState -> ColumnId -> TicketName -> TicketContent -> IO TicketId
createTicketImpl c (ColumnId cid) (TicketName name) (TicketContent content) = do
    freshTid <- U.nextRandom
    runClient c $ write cqlCreateTicket (params (cid, freshTid, name, content))
    pure $ TicketId freshTid

createTicketImpl' :: ColumnId -> TicketId -> TicketName -> TicketContent -> Client TicketId
createTicketImpl' (ColumnId cid) t@(TicketId tid) (TicketName name) (TicketContent content) = do
    write cqlCreateTicket (params (cid, tid, name, content))
    pure t

cqlCreateTicket :: PrepQuery W (UUID, UUID, Text, Text) ()
cqlCreateTicket =
    " INSERT INTO do_notation.ticket       \
    \ (columnid, id, name, content) VALUES \
    \ (       ?,  ?,    ?,       ?)        "

updateTicketImpl :: ClientState -> ColumnId -> TicketId -> TicketName -> TicketContent -> IO ()
updateTicketImpl c cid tid name content = runClient c $ updateTicketImpl' cid tid name content

updateTicketImpl' :: ColumnId -> TicketId -> TicketName -> TicketContent -> Client ()
updateTicketImpl' (ColumnId cid) (TicketId tid) (TicketName name) (TicketContent content) =
    write cqlUpdateTicket (params (name, content, cid, tid))
    where
    cqlUpdateTicket :: PrepQuery W (Text, Text, UUID, UUID) ()
    cqlUpdateTicket =
        " UPDATE do_notation.ticket \
        \ SET name    = ?,          \
        \     content = ?           \
        \ WHERE columnid = ?        \
        \   AND id       = ?        "

deleteTicketImpl :: ClientState -> ColumnId -> TicketId -> IO ()
deleteTicketImpl c cid tid = runClient c $ deleteTicketImpl' cid tid

deleteTicketImpl' :: ColumnId -> TicketId -> Client ()
deleteTicketImpl' (ColumnId cid) (TicketId tid) =
    write cqlDeleteTicket (params (cid, tid))
    where
    cqlDeleteTicket :: PrepQuery W (UUID, UUID) ()
    cqlDeleteTicket =
        " DELETE FROM do_notation.ticket \
        \ WHERE columnid = ?             \
        \ AND   id       = ?             " 

getColumnImpl :: ClientState -> ColumnId -> IO [Ticket]
getColumnImpl c (ColumnId cid) =
    runClient c $ map toTicket <$> query cqlGetColumn (params (Identity cid))
    where
    toTicket (ti, name, content) =
        Ticket (TicketId ti) (TicketName name) (TicketContent content)

    cqlGetColumn :: PrepQuery R (Identity UUID) (UUID, Text, Text)
    cqlGetColumn =
        " SELECT id, content, name \
        \ FROM do_notation.ticket  \
        \ WHERE columnId = ?       "

moveTicketImpl :: ClientState -> BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
moveTicketImpl c _ from to tid
    | from == to = pure ()
    | otherwise = runClient c $ do
        Ticket _ name content <- getTicketImpl' from tid
        _ <- createTicketImpl' to tid name content
        deleteTicketImpl' from tid

getTicketImpl :: ClientState -> ColumnId -> TicketId -> IO Ticket
getTicketImpl c cid tid = runClient c $ getTicketImpl' cid tid

getTicketImpl' :: ColumnId -> TicketId -> Client Ticket
getTicketImpl' (ColumnId cid) (TicketId tid) = query1 cqlGetTicket (params (cid, tid)) >>= \case
    Nothing -> error "no such ticket"
    Just (id_, name, content) -> pure $ Ticket (TicketId id_) (TicketName name) (TicketContent content)

cqlGetTicket :: PrepQuery R (UUID, UUID) (UUID, Text, Text)
cqlGetTicket =
    " SELECT id, name, content \
    \ FROM do_notation.ticket  \
    \ WHERE columnid = ?       \
    \ AND id = ?               "
