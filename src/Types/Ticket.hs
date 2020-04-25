{-# LANGUAGE DeriveGeneric #-}

module Types.Ticket where

import Data.Aeson
import Data.List.Split (splitOn)
import Data.Text       (Text)
import Data.UUID       (UUID)
import GHC.Generics    (Generic)

newtype TicketId =
    TicketId { ti_value :: UUID
             } deriving Generic

instance Show TicketId where
    show (TicketId tid) = show tid

instance ToJSON TicketId where
    toJSON = genericToJSON defaultOptions { unwrapUnaryRecords = True }

data Ticket =
    Ticket { t_id      :: !TicketId
           , t_name    :: !Text
           , t_content :: !Text
           } deriving (Generic, Show)

instance ToJSON Ticket where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

chop :: String -> String
chop = concat . drop 1 . splitOn "_"