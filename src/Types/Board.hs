{-# LANGUAGE DeriveGeneric #-}

module Types.Board where

import Types.Column
import Types.Json

import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text                  (Text)
import Data.Vector                (Vector)
import GHC.Generics               (Generic)

newtype BoardName =
    BoardName { bi_value :: Text
            } deriving Generic

instance FromJSON BoardName where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }

newtype Board =
    Board { b_columns :: Vector Column
          } deriving Generic

instance ToJSON Board where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

instance Show Board where
    show = unpack . encode
