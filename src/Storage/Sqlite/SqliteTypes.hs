module Storage.Sqlite.SqliteTypes ( SqlBoardId (..)
                                  , SqlBoardName (..)
                                  , SqlColumnId (..)
                                  , SqlColumnName (..)
                                  , SqlColumnPosition (..)
                                  , SqlColumnRow (..)
                                  , SqlUserId (..)
                                  , UserBoard (..)
                                  , UserRow (..)
                                  ) where

import Types.Board
import Types.BoardId
import Types.Column
import Types.Ticket
import Types.User

import Data.Maybe             (fromJust)
import Data.UUID              (fromText, toText)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

data UserRow =
    UserRow { ur_userid           :: !UserId
            , ur_salt             :: !Salt
            , ur_hashSaltPassword :: !HashedSaltedPassword
            }

instance ToRow UserRow where
  toRow (UserRow (UserId uid)
                 (Salt s)
                 (HashedSaltedPassword hspw)) =
      toRow (uid, s, hspw)

data UserBoard = UserBoard UserId BoardId

instance ToRow UserBoard where
  toRow (UserBoard (UserId userid)
                   (BoardId boardId)) =
      toRow (userid, toText boardId)

instance FromRow UserBoard where
  fromRow = UserBoard <$> (UserId                        <$> field)
                      <*> (BoardId . fromJust . fromText <$> field)

data TicketRow =
    TicketRow ColumnId TicketId TicketName TicketContent
        deriving (Eq, Ord)

instance ToRow TicketRow where
  toRow (TicketRow (ColumnId ci) 
                   (TicketId ti)
                   (TicketName n)
                   (TicketContent c)) =
      toRow (toText ci, toText ti, n, c)

instance FromRow TicketRow where
  fromRow = TicketRow <$> (ColumnId . fromJust . fromText <$> field)
                      <*> (TicketId . fromJust . fromText <$> field)
                      <*> (TicketName                     <$> field)
                      <*> (TicketContent                  <$> field)

newtype SqlColumnName =
  SqlColumnName ColumnName

instance ToField SqlColumnName where
  toField (SqlColumnName (ColumnName name)) = toField name

newtype SqlColumnPosition =
  SqlColumnPosition ColumnPosition

instance ToField SqlColumnPosition where
  toField (SqlColumnPosition (ColumnPosition pos)) = toField pos

newtype SqlColumnId =
  SqlColumnId ColumnId

instance ToField SqlColumnId where
  toField (SqlColumnId (ColumnId cid)) = toField . toText $ cid

newtype SqlUserId =
  SqlUserId UserId

instance ToRow SqlUserId where
  toRow (SqlUserId (UserId uid)) =
    toRow (Only uid)

newtype SqlBoardName =
  SqlBoardName BoardName

instance ToField SqlBoardName where
  toField (SqlBoardName (BoardName boardName)) = toField boardName

instance FromRow SqlBoardName where
  fromRow = SqlBoardName . BoardName <$> field

newtype SqlBoardId =
  SqlBoardId BoardId

instance ToField SqlBoardId where
  toField (SqlBoardId (BoardId boardId)) = toField . toText $ boardId

instance FromRow SqlBoardId where
  fromRow = SqlBoardId . BoardId . fromJust . fromText <$> field

instance ToRow SqlBoardId where
  toRow (SqlBoardId (BoardId bid)) =
    toRow (Only (toText bid))

data SqlColumnRow =
  SqlColumnRow ColumnPosition ColumnName ColumnId
    deriving (Eq, Ord)

instance FromRow SqlColumnRow where
  fromRow = SqlColumnRow <$> (ColumnPosition <$> field)
                         <*> (ColumnName <$> field)
                         <*> (ColumnId . fromJust . fromText <$> field)
