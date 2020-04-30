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
                      , getTicket         = getTicketImpl c
                      , getColumn         = getColumnImpl c
                      , moveTicket        = moveTicketImpl c
                      } 

createBoardColumnImpl :: Connection -> BoardName -> Int -> ColumnName -> IO ColumnId
createBoardColumnImpl c (BoardName boardName) position (ColumnName name) = do
    freshCid <- U.nextRandom
    execute c sqlCreateBoardColumn (BoardRow boardName position name (toText freshCid))
    pure $ ColumnId freshCid

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
            . V.map (\(BoardRow _ _ cn ci) -> Column (ColumnName cn) (ColumnId . fromJust . fromText $ ci))
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
    asColumnId :: [SqlColumnId] -> Maybe ColumnId
    asColumnId                [] = Nothing
    asColumnId [SqlColumnId _ x] = Just x
    asColumnId                xs = error $ "Too many column ids: " ++ show xs

    sqlGetDefaultColumn :: Query
    sqlGetDefaultColumn =
        " SELECT min(position), columnid \
        \ FROM board                     \
        \ WHERE name = ?                 "         

createTicketImpl :: Connection -> ColumnId -> Text -> Text -> IO TicketId
createTicketImpl c (ColumnId cid) name content = do
    freshTid <- U.nextRandom
    execute c sqlCreateTicket (toText cid, toText freshTid, name, content)
    pure $ TicketId freshTid

sqlCreateTicket :: Query
sqlCreateTicket =
    " INSERT INTO ticket                   \
    \ (columnid, id, name, content) VALUES \
    \ (       ?,  ?,    ?,       ?)        "

getColumnImpl :: Connection -> ColumnId -> IO [Ticket]
getColumnImpl c (ColumnId cid) =
    map toTicket <$> query c sqlGetColumn (Only $ toText cid)
    where
    toTicket (ti, name, content) = Ticket (TicketId $ fromJust . fromText $ ti) name content
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
    putTicket :: Text -> Text -> IO ()
    putTicket name content = execute c sqlCreateTicket (toText to, toText tid, name, content)

    removeFromColumn :: IO ()
    removeFromColumn = execute c sqlRemoveFromColumn (toText from, toText tid)

    sqlRemoveFromColumn :: Query
    sqlRemoveFromColumn =
        " DELETE FROM ticket \
        \ WHERE columnid = ? \
        \ AND id = ?         " 

getTicketImpl :: Connection -> ColumnId -> TicketId -> IO Ticket
getTicketImpl c (ColumnId cid) (TicketId tid) =
    query c sqlGetTicket (toText cid, toText tid) >>= \case
        []                     -> error $ "no such ticket: " ++ show (cid, tid)
        [(id_, name, content)] -> pure $ Ticket (TicketId . fromJust . fromText $ id_) name content
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
