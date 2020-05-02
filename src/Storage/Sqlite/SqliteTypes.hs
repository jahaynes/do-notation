module Storage.Sqlite.SqliteTypes where

import Types.Board
import Types.Column
import Types.Ticket

import Data.Maybe             (fromJust)
import Data.UUID              (fromText, toText)
import Database.SQLite.Simple (FromRow (..), ToRow (..), field)

data BoardRow =
    BoardRow BoardName ColumnPosition ColumnName ColumnId
        deriving Eq

instance ToRow BoardRow where
  toRow (BoardRow (BoardName name)
                  (ColumnPosition pos)
                  (ColumnName colName)
                  (ColumnId colId)) =
      toRow (name, pos, colName, toText colId)

instance FromRow BoardRow where
  fromRow = BoardRow <$> (BoardName                      <$> field)
                     <*> (ColumnPosition                 <$> field)
                     <*> (ColumnName                     <$> field)
                     <*> (ColumnId . fromJust . fromText <$> field)

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

{-
data SqlColumnId =
    SqlColumnId Int ColumnId

instance FromRow SqlColumnId where
  fromRow = do
      p  <- field
      ci <- field
      pure $
        SqlColumnId p (ColumnId . fromJust . fromText $ ci)
-}