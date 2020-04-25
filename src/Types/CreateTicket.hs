{-# LANGUAGE DeriveGeneric #-}

module Types.CreateTicket where

import Types.Board

import Data.Aeson
import Data.List.Split (splitOn)
import Data.Text       (Text)
import Data.UUID       (UUID)
import GHC.Generics    (Generic)

data CreateTicket =
    CreateTicket { ct_board   :: !BoardId
                 , ct_name    :: !Text
                 , ct_content :: !Text
                 } deriving Generic

instance FromJSON CreateTicket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

chop :: String -> String
chop = concat . drop 1 . splitOn "_"