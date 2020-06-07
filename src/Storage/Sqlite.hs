{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Storage.Sqlite where

import Errors
import Storage.Sqlite.SqliteTypes
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Column
import Types.Ticket
import Types.User

import           Control.Exception.Safe           (catch)
import           Data.List                   as L (sort)
import           Data.Maybe                       (catMaybes, fromJust)
import           Data.Text                        (Text)
import           Data.UUID                        (fromText, toText)
import qualified Data.UUID.V4                as U
import           Data.Vector                      (Vector)
import           Data.Vector.Algorithms.Heap as V (sort)
import qualified Data.Vector                 as V
import           Database.SQLite.Simple
import           Safe                             (headMay)

createSqlite :: FilePath -> IO StorageApi
createSqlite filename = do
    c <- open filename
    createSqlTables c
    pure $ StorageApi { createPassword     = createPasswordImpl c
                      , getSaltAndPassword = getSaltAndPasswordImpl c
                      , createUser         = createUserImpl c
                      , createBoard        = createBoardImpl c
                      , deleteBoard        = deleteBoardImpl c
                      , createColumn       = createColumnImpl c
                      , getBoard           = getBoardImpl c
                      , getBoards          = getBoardsImpl c
                      , getDefaultColumn   = getDefaultColumnImpl c
                      , createTicket       = createTicketImpl c
                      , updateTicket       = updateTicketImpl c
                      , deleteTicket       = deleteTicketImpl c
                      , getTicket          = getTicketImpl c
                      , getColumnTickets   = getColumnTicketsImpl c
                      , moveTicket         = moveTicketImpl c
                      }

createPasswordImpl :: Connection -> UserId -> Salt -> HashedSaltedPassword -> IO (Either ErrorResponse ())
createPasswordImpl c userId salt hashedSaltedPassword =
    catch (Right <$> execute c sqlCreatePassword (UserRow userId salt hashedSaltedPassword))
           errorHandler
    where
    sqlCreatePassword :: Query
    sqlCreatePassword =
        " INSERT INTO password                    \
        \ (userId, salt, hashsaltpassword) VALUES \
        \ (     ?,    ?,                ?)        "

    errorHandler e | sqlError e == ErrorConstraint = err' 409 "Could not create user.  User already taken?"

getSaltAndPasswordImpl :: Connection -> UserId -> IO (Maybe (Salt, HashedSaltedPassword))
getSaltAndPasswordImpl c (UserId userId) =
    handle <$> query c sqlGetSaltAndPassword (Only userId)
    where
    handle             [] = Nothing
    handle [(salt, hspw)] = Just (Salt salt, HashedSaltedPassword hspw)
    handle              _ = error "Too many userids"
    sqlGetSaltAndPassword :: Query
    sqlGetSaltAndPassword =
        " SELECT salt, hashsaltpassword \
        \ FROM password                 \
        \ WHERE userid = ?              "

createUserImpl :: Connection -> UserId -> BoardId -> IO ()
createUserImpl c userId boardId =
    execute c sqlCreateUser (UserBoard userId boardId)
    where
    sqlCreateUser :: Query
    sqlCreateUser =
        " INSERT INTO user         \
        \ (userId, boardid) VALUES \
        \ (     ?,       ?)        "

createBoardImpl :: Connection -> BoardName -> IO BoardId
createBoardImpl c boardName = do
    freshBid <- BoardId <$> U.nextRandom
    execute c sqlCreateBoard (SqlBoardId freshBid, SqlBoardName boardName)
    pure freshBid
    where
    sqlCreateBoard :: Query
    sqlCreateBoard =
        " INSERT INTO board           \
        \ (boardid, boardname) VALUES \
        \ (      ?,         ?)        "

deleteBoardImpl :: Connection -> BoardId -> IO ()
deleteBoardImpl c boardId = do
    execute c sqlDeleteUserBoard (Only (SqlBoardId boardId))
    execute c sqlDeleteBoard     (Only (SqlBoardId boardId))
    where
    sqlDeleteUserBoard :: Query
    sqlDeleteUserBoard =
        " DELETE FROM user  \
        \ WHERE boardid = ? "

    sqlDeleteBoard :: Query
    sqlDeleteBoard =
        " DELETE FROM board \
        \ WHERE boardid = ? "

createColumnImpl :: Connection -> BoardId -> ColumnPosition -> ColumnName -> IO ColumnId
createColumnImpl c boardId pos name = do
    freshCid <- ColumnId <$> U.nextRandom
    execute c sqlCreateColumn ( SqlBoardId boardId
                              , SqlColumnId freshCid
                              , SqlColumnPosition pos
                              , SqlColumnName name )
    pure freshCid
    where
    sqlCreateColumn :: Query
    sqlCreateColumn =
        " INSERT INTO column                               \
        \ (boardid, columnid, position, columnname) VALUES \
        \ (      ?,        ?,        ?,          ?)        "

getBoardImpl :: Connection -> BoardId -> IO (Maybe Board)
getBoardImpl c boardId = 
    query c sqlGetBoard (Only (SqlBoardId boardId)) >>= \case
        []                       -> pure Nothing
        [SqlBoardName boardName] -> do
            columns <- asColumns <$> query c sqlGetColumns (Only (SqlBoardId boardId))
            pure . Just $ Board boardId boardName columns
        _                        -> error "too many boardnames"
    where
    asColumns :: [SqlColumnRow] -> Vector Column
    asColumns = V.map asColumn
              . V.modify V.sort
              . V.fromList
        where
        asColumn :: SqlColumnRow -> Column
        asColumn (SqlColumnRow _ name columnId) = Column boardId columnId name

    sqlGetBoard :: Query
    sqlGetBoard =
        " SELECT boardname  \
        \ FROM board        \
        \ WHERE boardid = ? "

    sqlGetColumns :: Query
    sqlGetColumns =
        " SELECT position, columnname, columnid \
        \ FROM column                           \
        \ WHERE boardid = ?                     "

getBoardsImpl :: Connection -> UserId -> IO [(BoardId, BoardName)]
getBoardsImpl c userId = do
    boardIds <- map handle <$> query c sqlGetBoards (SqlUserId userId)
    catMaybes <$> mapM (withBoardName c) boardIds
    where
    handle :: SqlBoardId -> BoardId
    handle (SqlBoardId boardId) = boardId

    sqlGetBoards :: Query
    sqlGetBoards =
        " SELECT boardid   \
        \ FROM user        \
        \ WHERE userid = ? "

withBoardName :: Connection -> BoardId -> IO (Maybe (BoardId, BoardName))
withBoardName c boardId =
    handle <$> query c sqlGetBoardName (SqlBoardId boardId)
    where
    handle :: [SqlBoardName] -> Maybe (BoardId, BoardName)
    handle                       [] = Nothing
    handle [SqlBoardName boardName] = Just (boardId, boardName)
    handle                        _ = error "Too many boardnames"
    sqlGetBoardName :: Query
    sqlGetBoardName =
        " SELECT boardname  \
        \ FROM board        \
        \ WHERE boardid = ? "

getDefaultColumnImpl :: Connection -> BoardId -> IO (Maybe ColumnId)
getDefaultColumnImpl c boardId =
    asColumnId <$> query c sqlGetDefaultColumn (Only (SqlBoardId boardId))
    where
    asColumnId :: [(Int, Text)] -> Maybe ColumnId
    asColumnId = headMay
               . map (\(_, columnId) -> ColumnId . fromJust . fromText $ columnId)
               . L.sort

    sqlGetDefaultColumn :: Query
    sqlGetDefaultColumn =
        " SELECT position, columnid \
        \ FROM column               \
        \ WHERE boardId = ?         "

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

updateTicketImpl :: Connection -> ColumnId -> TicketId -> TicketName -> TicketContent -> IO ()
updateTicketImpl c (ColumnId cid) (TicketId tid) (TicketName name) (TicketContent content) =
    execute c sqlUpdateTicket (name, content, toText cid, toText tid)
    where
    sqlUpdateTicket :: Query
    sqlUpdateTicket =
        " UPDATE ticket      \
        \ SET name    = ?,   \
        \     content = ?    \
        \ WHERE columnid = ? \
        \   AND id       = ? "

deleteTicketImpl :: Connection -> ColumnId -> TicketId -> IO ()
deleteTicketImpl c (ColumnId cid) (TicketId tid) =
    execute c sqlDeleteTicket (toText cid, toText tid)
    where
    sqlDeleteTicket :: Query
    sqlDeleteTicket =
        " DELETE FROM ticket \
        \ WHERE columnId = ? \
        \ AND   id       = ? "

getColumnTicketsImpl :: Connection -> ColumnId -> IO [Ticket]
getColumnTicketsImpl c (ColumnId cid) =
    map toTicket <$> query c sqlGetColumn (Only $ toText cid)
    where
    toTicket (ti, name, content) =
        Ticket (TicketId $ fromJust . fromText $ ti) (TicketName name) (TicketContent content) --todo duplicate
    sqlGetColumn :: Query
    sqlGetColumn =
        " SELECT id, content, name \
        \ FROM ticket              \
        \ WHERE columnId = ?       "

moveTicketImpl :: Connection -> BoardName -> ColumnId -> ColumnId -> TicketId -> IO (Either ErrorResponse ())
moveTicketImpl c _ (ColumnId from) (ColumnId to) (TicketId tid)
    | from == to = pure $ Right ()
    | otherwise =
        getTicketImpl c (ColumnId from) (TicketId tid) >>= \case
            Nothing -> err' 404 "No such ticket to move"
            Just (Ticket _ name content) -> do
                putTicket name content
                removeFromColumn
                pure $ Right ()

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

getTicketImpl :: Connection -> ColumnId -> TicketId -> IO (Maybe Ticket)
getTicketImpl c (ColumnId cid) (TicketId tid) =
    query c sqlGetTicket (toText cid, toText tid) >>= \case
        []                     -> pure Nothing
        [(id_, name, content)] -> pure . Just $ Ticket (TicketId . fromJust . fromText $ id_) (TicketName name) (TicketContent content)
        _                      -> error "Too many tickets"

sqlGetTicket :: Query
sqlGetTicket =
    " SELECT id, name, content \
    \ FROM ticket              \
    \ WHERE columnid = ?       \
    \ AND id = ?               "

createSqlTables :: Connection -> IO ()
createSqlTables c = do
    execute_ c schemaCreateUser
    execute_ c schemaCreatePassword
    execute_ c schemaCreateBoard
    execute_ c schemaCreateColumn
    execute_ c schemaCreateTicket
    where

    schemaCreateUser :: Query
    schemaCreateUser =
        " CREATE TABLE IF NOT EXISTS          \
        \   user                              \
        \     ( userid  TEXT                  \
        \     , boardid UUID                  \
        \     , PRIMARY KEY (userid, boardid) \
        \     )                               "

    schemaCreatePassword :: Query
    schemaCreatePassword =
        " CREATE TABLE IF NOT EXISTS    \
        \   password                    \
        \     ( userid TEXT PRIMARY KEY \
        \     , salt             BLOB   \
        \     , hashsaltpassword BLOB   \
        \     )                         "

    schemaCreateBoard :: Query
    schemaCreateBoard = 
        " CREATE TABLE IF NOT EXISTS             \
        \   board                                \
        \     ( boardid   UUID                   \
        \     , boardname TEXT                   \
        \     , PRIMARY KEY (boardid, boardname) \
        \     )                                  "

    schemaCreateColumn :: Query
    schemaCreateColumn = 
        " CREATE TABLE IF NOT EXISTS            \
        \   column                              \
        \     ( boardid     UUID                \
        \     , columnid    UUID                \
        \     , position    INT                 \
        \     , columnname  TEXT                \
        \     , PRIMARY KEY (boardid, columnid) \
        \     )                                 "

    schemaCreateTicket :: Query
    schemaCreateTicket = 
        " CREATE TABLE IF NOT EXISTS       \
        \   ticket                         \
        \     ( columnid UUID              \
        \     , id       UUID              \
        \     , name     TEXT              \
        \     , content  TEXT              \
        \     , PRIMARY KEY (columnid, id) \
        \     )                            "
