{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module Storage.Cassandra.Queries where

import Storage.Cassandra.Common (params)
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Ticket

import           Data.Int                    (Int32)
import           Data.Text                   (Text)
import qualified Data.Vector           as V
import           Data.Vector.Algorithms.Heap (sort)
import           Data.UUID                   (UUID)
import qualified Data.UUID.V4          as U
import           Database.CQL.IO 

createApi :: ClientState -> StorageApi
createApi c = 
    StorageApi { createBoardColumn = createBoardColumnImpl c
               , getBoard          = getBoardImpl c
               , getDefaultColumn  = getDefaultColumnImpl c
               , getColumn         = getColumnImpl c
               , createTicket      = createTicketImpl c
               , moveTicket        = moveTicketImpl c
               }

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

createBoardColumnImpl :: ClientState -> BoardName -> Int32 -> ColumnName -> IO ColumnId
createBoardColumnImpl c (BoardName boardName) position (ColumnName name) = do
    freshCid <- U.nextRandom
    runClient c $ write cqlCreateBoardColumn (params (boardName, position, name, freshCid))
    pure $ ColumnId freshCid

    where
    cqlCreateBoardColumn :: PrepQuery W (Text, Int32, Text, UUID) ()
    cqlCreateBoardColumn =
        " INSERT INTO do_notation.board                 \
        \ (name, position, columnname, columnid) VALUES \
        \ (   ?,        ?,          ?,        ?)        "

createTicketImpl :: ClientState -> ColumnId -> Text -> Text -> IO TicketId
createTicketImpl c (ColumnId cid) name content = do
    freshTid <- U.nextRandom
    runClient c $ write cqlCreateTicket (params (cid, freshTid, name, content))
    pure $ TicketId freshTid

getColumnImpl :: ClientState -> ColumnId -> IO [Ticket]
getColumnImpl c (ColumnId cid) =
    runClient c $ map toTicket <$> query cqlGetColumn (params (Identity cid))
    where
    toTicket (ti, name, content) = Ticket (TicketId ti) name content

    cqlGetColumn :: PrepQuery R (Identity UUID) (UUID, Text, Text)
    cqlGetColumn =
        " SELECT id, content, name \
        \ FROM do_notation.ticket  \
        \ WHERE columnId = ?       "

cqlCreateTicket :: PrepQuery W (UUID, UUID, Text, Text) ()
cqlCreateTicket =
    " INSERT INTO do_notation.ticket       \
    \ (columnid, id, name, content) VALUES \
    \ (       ?,  ?,    ?,       ?)        "

moveTicketImpl :: ClientState -> BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
moveTicketImpl c _ (ColumnId from) (ColumnId to) (TicketId tid)
    | from == to = pure ()
    | otherwise = runClient c $ do
        (name, content) <- getTicket
        putTicket name content
        removeFromColumn

    where
    getTicket :: Client (Text, Text)
    getTicket = query1 cqlGetTicket (params (from, tid)) >>= \case
        Nothing -> error "no such ticket"
        Just x  -> pure x

    putTicket :: Text -> Text -> Client ()
    putTicket name content = write cqlCreateTicket (params (to, tid, name, content))

    removeFromColumn :: Client ()
    removeFromColumn = write cqlRemoveFromColumn (params (from, tid))

    cqlRemoveFromColumn :: PrepQuery W (UUID, UUID) ()
    cqlRemoveFromColumn =
        " DELETE FROM do_notation.ticket \
        \ WHERE columnid = ? and id = ?  " 

    cqlGetTicket :: PrepQuery R (UUID, UUID) (Text, Text)
    cqlGetTicket =
        " SELECT name, content    \
        \ FROM do_notation.ticket \
        \ WHERE columnid = ?      \
        \ AND id = ?              "
