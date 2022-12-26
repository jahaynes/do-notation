{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric   #-}

module Types.Board ( Board (..)
                   , BoardName (..)
                   ) where

import Types.BoardId
import Types.Column
import Types.Json

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text       (Text)
import Data.Vector     (Vector)
import GHC.Generics    (Generic)

newtype BoardName =
    BoardName { bi_value :: Text
              } deriving (Eq, Generic, NFData)

instance ToJSON BoardName where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

instance FromJSON BoardName where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }

data Board =
    Board { b_id      :: !BoardId
          , b_name    :: !BoardName
          , b_columns :: !(Vector Column)
          } deriving (Generic, NFData)

instance ToJSON Board where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }
