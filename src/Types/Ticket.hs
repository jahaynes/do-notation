{-# LANGUAGE DeriveGeneric #-}

module Types.Ticket where

import Types.Json

import Data.Aeson
import Data.Text       (Text)
import Data.UUID       (UUID)
import GHC.Generics    (Generic)

newtype TicketId =
    TicketId { ti_value :: UUID
             } deriving Generic

instance Show TicketId where
    show (TicketId tid) = show tid

instance ToJSON TicketId where
    toJSON (TicketId u) = toJSON u

instance FromJSON TicketId where
    parseJSON u = TicketId <$> parseJSON u

data Ticket =
    Ticket { t_id      :: !TicketId
           , t_name    :: !Text
           , t_content :: !Text
           } deriving (Generic, Show)

instance ToJSON Ticket where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }
