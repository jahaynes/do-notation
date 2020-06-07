{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module Storage.Cassandra.Queries where

import Errors
import Storage.Cassandra.Common (params)
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Column
import Types.Ticket
import Types.User

import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Int                           (Int32)
import           Data.List                   as L   (sort)
import           Data.Maybe                         (catMaybes)
import           Data.Text                          (Text)
import           Data.Vector                        (Vector)
import qualified Data.Vector                 as V
import           Data.Vector.Algorithms.Heap as V   (sort)
import           Data.UUID                          (UUID)
import qualified Data.UUID.V4                as U
import           Database.CQL.IO 
import           Database.CQL.Protocol              (Blob (Blob))
import           Safe                               (headMay)

createApi :: ClientState -> StorageApi
createApi c = 
    StorageApi { createPassword     = createPasswordImpl c
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

blob :: ByteString -> Blob
blob = Blob . LBS.fromStrict

createPasswordImpl :: ClientState -> UserId -> Salt -> HashedSaltedPassword -> IO (Either ErrorResponse ())
createPasswordImpl c (UserId userId) (Salt salt) (HashedSaltedPassword hspw) =
    -- TODO find and catch errors here, instead of always Right
    runClient c $ Right <$> write cqlCreatePassword (params (userId, blob salt, blob hspw))
    where
    cqlCreatePassword :: PrepQuery W (Text, Blob, Blob) ()
    cqlCreatePassword =
        " INSERT INTO do_notation.password        \
        \ (userid, salt, hashsaltpassword) VALUES \
        \ (     ?,    ?,                ?)        "

getSaltAndPasswordImpl :: ClientState -> UserId -> IO (Maybe (Salt, HashedSaltedPassword))
getSaltAndPasswordImpl c (UserId userid) =
    handle <$> runClient c (query cqlGetSaltAndPassword (defQueryParams One (Identity userid)))
    where
    handle                       [] = Nothing
    handle [(Blob salt, Blob hspw)] = Just (Salt $ LBS.toStrict salt, HashedSaltedPassword $ LBS.toStrict hspw)
    handle                        _ = error "Too many userid"

    cqlGetSaltAndPassword :: PrepQuery R (Identity Text) (Blob, Blob)
    cqlGetSaltAndPassword =
        " SELECT salt, hashsaltpassword \
        \ FROM do_notation.password     \
        \ WHERE userid = ?              "

createUserImpl :: ClientState -> UserId -> BoardId -> IO ()
createUserImpl c (UserId uid) (BoardId bid) =
    runClient c $ write cqlCreateUser (params (uid, bid))
    where
    cqlCreateUser :: PrepQuery W (Text, UUID) ()
    cqlCreateUser =
        " INSERT INTO do_notation.user \
        \ (userid, boardid) VALUES     \
        \ (     ?,       ?)            "

getBoardImpl :: ClientState -> BoardId -> IO (Maybe Board)
getBoardImpl c bi@(BoardId boardId) = runClient c $ do
    mBoardName <- query1 cqlGetBoard (defQueryParams One (Identity boardId))
    case mBoardName of
        Nothing            -> pure Nothing
        Just (Identity bn) -> do
            columns <- asColumns <$> query cqlGetColumns (defQueryParams One (Identity boardId))
            pure . Just $ Board bi (BoardName bn) columns

    where
    asColumns :: [(Int32, Text, UUID)] -> Vector Column
    asColumns = V.map (\(_, cn, ci) -> Column bi (ColumnId ci) (ColumnName cn))
              . V.modify V.sort
              . V.fromList

    cqlGetBoard :: PrepQuery R (Identity UUID) (Identity Text)
    cqlGetBoard =
        " SELECT boardname       \
        \ FROM do_notation.board \
        \ WHERE boardid = ?      "

    cqlGetColumns :: PrepQuery R (Identity UUID) (Int32, Text, UUID)
    cqlGetColumns =
        " SELECT position, columnname, columnid \
        \ FROM do_notation.column               \
        \ WHERE boardid = ?                     "

getBoardsImpl :: ClientState -> UserId -> IO [(BoardId, BoardName)]
getBoardsImpl c (UserId uid) = runClient c $ do
    boardIds <- fmap toBoardId <$> query cqlGetBoards (defQueryParams One (Identity uid))
    catMaybes <$> mapM withBoardName boardIds
    where
    toBoardId (Identity uuid) = BoardId uuid
    cqlGetBoards :: PrepQuery R (Identity Text) (Identity UUID)
    cqlGetBoards =
        " SELECT boardid        \
        \ FROM do_notation.user \
        \ WHERE userid = ?      "

withBoardName :: BoardId -> Client (Maybe (BoardId, BoardName))
withBoardName b@(BoardId bi) =
    fmap toBoardName <$> query1 cqlGetBoardName (defQueryParams One (Identity bi))
    where
    toBoardName (Identity txt) = (b, BoardName txt)
    cqlGetBoardName :: PrepQuery R (Identity UUID) (Identity Text)
    cqlGetBoardName =
        " SELECT boardname       \
        \ FROM do_notation.board \
        \ WHERE boardid = ?      "

createBoardImpl :: ClientState -> BoardName -> IO BoardId
createBoardImpl c (BoardName name) = do
    freshBid <- U.nextRandom
    runClient c $ write cqlCreateBoard (params (freshBid, name))
    pure $ BoardId freshBid

    where
    cqlCreateBoard :: PrepQuery W (UUID, Text) ()
    cqlCreateBoard =
        " INSERT INTO do_notation.board \
        \ (boardid, boardname) VALUES   \
        \ (      ?,         ?)          "

deleteBoardImpl :: ClientState -> BoardId -> IO ()
deleteBoardImpl c (BoardId boardId) = runClient c $ do
    write cqlDeleteUserBoard (params (Identity boardId))
    write cqlDeleteBoard     (params (Identity boardId))
    where
    cqlDeleteUserBoard :: PrepQuery W (Identity UUID) ()
    cqlDeleteUserBoard =
        " DELETE FROM do_notation.column \
        \ WHERE boardid = ?              "

    cqlDeleteBoard :: PrepQuery W (Identity UUID) ()
    cqlDeleteBoard =
        " DELETE FROM do_notation.board \
        \ WHERE boardid = ?             "

getDefaultColumnImpl :: ClientState -> BoardId -> IO (Maybe ColumnId)
getDefaultColumnImpl c (BoardId boardId) = runClient c $
    asColumnId <$> query cqlGetDefaultColumn (params (Identity boardId))

    where
    asColumnId :: [(Int32, UUID)] -> Maybe ColumnId
    asColumnId = headMay
               . map (\(_, columnId) -> ColumnId columnId)
               . L.sort

    cqlGetDefaultColumn :: PrepQuery R (Identity UUID) (Int32, UUID)
    cqlGetDefaultColumn =
        " SELECT position, columnid \
        \ FROM column               \
        \ WHERE boardId = ?         "

createColumnImpl :: ClientState -> BoardId -> ColumnPosition -> ColumnName -> IO ColumnId
createColumnImpl c (BoardId boardId) (ColumnPosition pos) (ColumnName name) = do
    freshCid <- U.nextRandom
    runClient c $ write cqlCreateColumn (params (boardId, freshCid, fromIntegral pos, name))
    pure $ ColumnId freshCid

    where
    cqlCreateColumn :: PrepQuery W (UUID, UUID, Int32, Text) ()
    cqlCreateColumn =
        " INSERT INTO do_notation.column                   \
        \ (boardid, columnid, position, columnname) VALUES \
        \ (      ?,        ?,        ?,          ?)        "

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

getColumnTicketsImpl :: ClientState -> ColumnId -> IO [Ticket]
getColumnTicketsImpl c (ColumnId cid) =
    runClient c $ map toTicket <$> query cqlGetColumn (params (Identity cid))
    where
    toTicket (ti, name, content) =
        Ticket (TicketId ti) (TicketName name) (TicketContent content)

    cqlGetColumn :: PrepQuery R (Identity UUID) (UUID, Text, Text)
    cqlGetColumn =
        " SELECT id, content, name \
        \ FROM do_notation.ticket  \
        \ WHERE columnId = ?       "

moveTicketImpl :: ClientState -> BoardName -> ColumnId -> ColumnId -> TicketId -> IO (Either ErrorResponse ())
moveTicketImpl c _ from to tid
    | from == to = pure $ Right ()
    | otherwise = runClient c $
        getTicketImpl' from tid >>= \case
            Nothing -> err' 404 "No such ticket to move"
            Just (Ticket _ name content) -> do
                _ <- createTicketImpl' to tid name content
                deleteTicketImpl' from tid
                pure $ Right ()

getTicketImpl :: ClientState -> ColumnId -> TicketId -> IO (Maybe Ticket)
getTicketImpl c cid tid = runClient c $ getTicketImpl' cid tid

getTicketImpl' :: ColumnId -> TicketId -> Client (Maybe Ticket)
getTicketImpl' (ColumnId cid) (TicketId tid) =
    query1 cqlGetTicket (params (cid, tid)) >>= \case
        Nothing -> pure Nothing
        Just (id_, name, content) -> pure . Just $ Ticket (TicketId id_) (TicketName name) (TicketContent content)

cqlGetTicket :: PrepQuery R (UUID, UUID) (UUID, Text, Text)
cqlGetTicket =
    " SELECT id, name, content \
    \ FROM do_notation.ticket  \
    \ WHERE columnid = ?       \
    \ AND id = ?               "
