{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Storage.Sqlite where

import Storage.Sqlite.SqliteTypes
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Ticket

import           Data.Maybe                       (fromJust)
import           Data.Ord
import           Data.Text                        (Text)
import           Data.UUID                        (fromText, toText)
import qualified Data.UUID.V4                as U
import           Data.Vector.Algorithms.Heap      (sortBy)
import qualified Data.Vector                 as V
import           Database.SQLite.Simple

createSqlite :: FilePath -> IO StorageApi
createSqlite filename = do
    c <- open filename
    createSqlTables c
    pure $ StorageApi { createBoardColumn = createBoardColumnImpl c
                      , getBoard          = getBoardImpl c
                      , getDefaultColumn  = getDefaultColumnImpl c
                      , createTicket      = createTicketImpl c
                      , deleteTicket      = deleteTicketImpl c
                      , getTicket         = getTicketImpl c
                      , getColumn         = getColumnImpl c
                      , moveTicket        = moveTicketImpl c
                      } 

createBoardColumnImpl :: Connection -> BoardName -> ColumnPosition -> ColumnName -> IO ColumnId
createBoardColumnImpl c boardName columnPosition colName = do
    freshCid <- ColumnId <$> U.nextRandom
    execute c sqlCreateBoardColumn (BoardRow boardName columnPosition colName freshCid)
    pure freshCid

    where
    sqlCreateBoardColumn :: Query
    sqlCreateBoardColumn =
        " INSERT INTO board                             \
        \ (name, position, columnname, columnid) VALUES \
        \ (   ?,        ?,          ?,        ?)        "

getBoardImpl :: Connection -> BoardName -> IO Board
getBoardImpl c (BoardName boardName) =
    asBoard <$> query c sqlGetBoard (Only boardName)
    where
    asBoard :: [BoardRow] -> Board
    asBoard = Board
            . V.map (\(BoardRow _ _ cn ci) -> Column cn ci)
            . V.modify (sortBy . comparing $ \(BoardRow _ pos _ _) -> pos)
            . V.fromList

    sqlGetBoard :: Query
    sqlGetBoard =
        " SELECT name, position, columnname, columnid \
        \ FROM board                                  \
        \ WHERE name = ?                              "

getDefaultColumnImpl :: Connection -> BoardName -> IO (Maybe ColumnId)
getDefaultColumnImpl c (BoardName boardName) =
    asColumnId <$> query c sqlGetDefaultColumn (Only boardName)

    where
    asColumnId :: [(Int, Text)] -> Maybe ColumnId
    asColumnId       [] = Nothing
    asColumnId [(_, x)] = ColumnId <$> fromText x
    asColumnId       xs = error $ "Too many column ids: " ++ show xs

    sqlGetDefaultColumn :: Query
    sqlGetDefaultColumn =
        " SELECT min(position), columnid \
        \ FROM board                     \
        \ WHERE name = ?                 "         

createTicketImpl :: Connection -> ColumnId -> TicketName -> TicketContent -> IO TicketId
createTicketImpl c (ColumnId cid) (TicketName name) (TicketContent content) = do
    freshTid <- U.nextRandom
    execute c sqlCreateTicket (toText cid, toText freshTid, name, content)
    pure $ TicketId freshTid

sqlCreateTicket :: Query
sqlCreateTicket =
    " INSERT INTO ticket                   \
    \ (columnid, id, name, content) VALUES \
    \ (       ?,  ?,    ?,       ?)        "

deleteTicketImpl :: Connection -> ColumnId -> TicketId -> IO ()
deleteTicketImpl c (ColumnId cid) (TicketId tid) =
    execute c sqlDeleteTicket (toText cid, toText tid)
    where
    sqlDeleteTicket :: Query
    sqlDeleteTicket =
        " DELETE FROM ticket \
        \ WHERE columnId = ? \
        \ AND   id       = ? "

getColumnImpl :: Connection -> ColumnId -> IO [Ticket]
getColumnImpl c (ColumnId cid) =
    map toTicket <$> query c sqlGetColumn (Only $ toText cid)
    where
    toTicket (ti, name, content) =
        Ticket (TicketId $ fromJust . fromText $ ti) (TicketName name) (TicketContent content) --todo duplicate
    sqlGetColumn :: Query
    sqlGetColumn =
        " SELECT id, content, name \
        \ FROM ticket              \
        \ WHERE columnId = ?       "

moveTicketImpl :: Connection -> BoardName -> ColumnId -> ColumnId -> TicketId -> IO ()
moveTicketImpl c _ (ColumnId from) (ColumnId to) (TicketId tid)
    | from == to = pure ()
    | otherwise = do
        Ticket _ name content <- getTicketImpl c (ColumnId from) (TicketId tid)
        putTicket name content
        removeFromColumn

    where
    putTicket :: TicketName -> TicketContent -> IO ()
    putTicket (TicketName name) (TicketContent content) =
        execute c sqlCreateTicket (toText to, toText tid, name, content)

    removeFromColumn :: IO ()
    removeFromColumn =
        execute c sqlRemoveFromColumn (toText from, toText tid)

    sqlRemoveFromColumn :: Query
    sqlRemoveFromColumn =
        " DELETE FROM ticket \
        \ WHERE columnid = ? \
        \ AND id = ?         " 

getTicketImpl :: Connection -> ColumnId -> TicketId -> IO Ticket
getTicketImpl c (ColumnId cid) (TicketId tid) =
    query c sqlGetTicket (toText cid, toText tid) >>= \case
        []                     -> error $ "no such ticket: " ++ show (cid, tid)
        [(id_, name, content)] -> pure $ Ticket (TicketId . fromJust . fromText $ id_) (TicketName name) (TicketContent content)
        _                      -> error "Too many tickets"

sqlGetTicket :: Query
sqlGetTicket =
    " SELECT id, name, content \
    \ FROM ticket              \
    \ WHERE columnid = ?       \
    \ AND id = ?               "

createSqlTables :: Connection -> IO ()
createSqlTables c = do
    execute_ c schemaCreateBoard
    execute_ c schemaCreateTicket
    where
    schemaCreateBoard :: Query
    schemaCreateBoard = 
        " CREATE TABLE IF NOT EXISTS \
        \   board                    \
        \     ( name       TEXT      \
        \     , position   INT       \
        \     , columnName TEXT      \
        \     , columnId   UUID      \
        \     )                      "

    schemaCreateTicket :: Query
    schemaCreateTicket = 
        " CREATE TABLE IF NOT EXISTS \
        \   ticket                   \
        \     ( columnId UUID        \
        \     , id       UUID        \
        \     , name     TEXT        \
        \     , content  TEXT        \
        \     )                      "
