{-# LANGUAGE DeriveGeneric #-}

module Types.Column where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split            (splitOn)
import Data.Text                  (Text)
import Data.UUID                  (UUID)
import GHC.Generics               (Generic)

data Column = 
    Column { c_name :: !ColumnName
           , c_id   :: !ColumnId
           } deriving Generic

instance ToJSON Column where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

instance Show Column where
    show = unpack . encode

newtype ColumnId =
    ColumnId { cn_id :: UUID
             }

instance ToJSON ColumnId where
    toJSON (ColumnId i) = toJSON i

instance Show ColumnId where
    show (ColumnId cid) = show cid

newtype ColumnName =
    ColumnName { cn_value :: Text
               } deriving (Eq, Ord)

instance ToJSON ColumnName where
    toJSON (ColumnName cn) = toJSON cn

instance ToJSONKey ColumnName where
    toJSONKey = toJSONKeyText cn_value

chop :: String -> String
chop = concat . drop 1 . splitOn "_"