{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric   #-}

module Types.BoardId (BoardId (..)) where

import Types.Json

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.UUID       (UUID)
import GHC.Generics    (Generic)
import Servant         (FromHttpApiData (..))

newtype BoardId =
    BoardId { bn_value :: UUID
            } deriving (Eq, Generic, NFData)

instance FromJSON BoardId where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }

instance ToJSON BoardId where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

instance FromHttpApiData BoardId where
    parseUrlPiece uuid = BoardId <$> parseUrlPiece uuid