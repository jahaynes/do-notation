{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric   #-}

module Types.Column where

import Types.Json

import Control.DeepSeq            (NFData)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text                  (Text)
import Data.UUID                  (UUID)
import GHC.Generics               (Generic)

data Column = 
    Column { c_name :: !ColumnName
           , c_id   :: !ColumnId
           } deriving (Generic, NFData)

instance ToJSON Column where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

instance Show Column where
    show = unpack . encode

newtype ColumnPosition =
    ColumnPosition { cp_position :: Int
                   } deriving (Eq, Ord, Show)

newtype ColumnId =
    ColumnId { cn_id :: UUID
             } deriving (Eq, Ord, Generic, NFData)

instance ToJSON ColumnId where
    toJSON (ColumnId i) = toJSON i

instance FromJSON ColumnId where
    parseJSON u = ColumnId <$> parseJSON u

instance Show ColumnId where
    show (ColumnId cid) = show cid

newtype ColumnName =
    ColumnName { cn_value :: Text
               } deriving (Eq, Ord, Generic, NFData)

instance ToJSON ColumnName where
    toJSON (ColumnName cn) = toJSON cn

instance ToJSONKey ColumnName where
    toJSONKey = toJSONKeyText cn_value
