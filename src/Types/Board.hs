{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric   #-}

module Types.Board where

import Types.Column
import Types.Json

import Control.DeepSeq            (NFData)
import Data.Aeson
import Data.Text                  (Text)
import Data.Vector                (Vector)
import GHC.Generics               (Generic)
import Servant                    (FromHttpApiData (..))

newtype BoardName =
    BoardName { bi_value :: Text
            } deriving (Eq, Generic)

instance FromJSON BoardName where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }

instance FromHttpApiData BoardName where
    parseUrlPiece txt = BoardName <$> parseUrlPiece txt

newtype Board =
    Board { b_columns :: Vector Column
          } deriving (Generic, NFData)

instance ToJSON Board where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }
