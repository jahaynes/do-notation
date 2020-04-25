{-# LANGUAGE DeriveGeneric #-}

module Types.Board where

import Types.Column

import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text                  (Text)
import Data.Vector                (Vector)
import GHC.Generics               (Generic)

newtype BoardId =
    BoardId { bi_value :: Text
            } deriving Generic

instance FromJSON BoardId where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }

newtype Board =
    Board { columns :: Vector Column
          } deriving Generic

instance ToJSON Board where
    toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True
                                          }

instance Show Board where
    show = unpack . encode
