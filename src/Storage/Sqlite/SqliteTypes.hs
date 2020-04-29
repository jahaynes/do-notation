module Storage.Sqlite.SqliteTypes where

import Types.Column

import Data.Maybe             (fromJust)
import Data.Text              (Text)
import Data.UUID              (fromText)
import Database.SQLite.Simple (FromRow (..), ToRow (..), field)

data BoardRow =
    BoardRow Text Int Text Text
        deriving (Eq, Ord)

instance ToRow BoardRow where
  toRow (BoardRow name pos colName colId) =
      toRow (name, pos, colName, colId)

instance FromRow BoardRow where
  fromRow = BoardRow <$> field
                     <*> field
                     <*> field
                     <*> field

data TicketRow =
    TicketRow Text Text Text Text
        deriving (Eq, Ord, Show)

instance ToRow TicketRow where
  toRow (TicketRow columnId id_ name content) =
      toRow (columnId, id_, name, content)

instance FromRow TicketRow where
  fromRow = TicketRow <$> field
                      <*> field
                      <*> field
                      <*> field

data SqlColumnId =
    SqlColumnId Int ColumnId deriving Show

instance FromRow SqlColumnId where
  fromRow = do
      p  <- field
      ci <- field
      pure $
        SqlColumnId p (ColumnId . fromJust . fromText $ ci)