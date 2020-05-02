{-# LANGUAGE DeriveGeneric #-}

module Types.Ticket where

import Types.Json

import Data.Aeson
import Data.Text       (Text)
import Data.UUID       (UUID)
import GHC.Generics    (Generic)

newtype TicketId =
    TicketId { ti_value :: UUID
             } deriving (Generic, Eq, Ord)

instance ToJSON TicketId where
    toJSON (TicketId u) = toJSON u

instance FromJSON TicketId where
    parseJSON u = TicketId <$> parseJSON u

newtype TicketName =
    TicketName { tn_value :: Text
               } deriving (Eq, Ord)

instance ToJSON TicketName where
    toJSON (TicketName n) = toJSON n

instance FromJSON TicketName where
    parseJSON u = TicketName <$> parseJSON u

newtype TicketContent =
    TicketContent { tc_value :: Text
                  } deriving (Eq, Ord)

instance ToJSON TicketContent where
    toJSON (TicketContent c) = toJSON c

instance FromJSON TicketContent where
    parseJSON u = TicketContent <$> parseJSON u

data Ticket =
    Ticket { t_id      :: !TicketId
           , t_name    :: !TicketName
           , t_content :: !TicketContent
           } deriving Generic

instance ToJSON Ticket where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }
