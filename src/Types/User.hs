module Types.User where

import Types.BoardId

import Data.Aeson      (FromJSON, parseJSON)
import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Vector     (Vector)

newtype UserId =
    UserId Text

instance FromJSON UserId where
    parseJSON u = UserId <$> parseJSON u

data User =
    User { u_id       :: !UserId
         , u_boardIds :: !(Vector BoardId)
         }

newtype RawPassword =
    RawPassword Text

instance FromJSON RawPassword where
    parseJSON p = RawPassword <$> parseJSON p

newtype Salt =
    Salt ByteString

newtype HashedSaltedPassword = 
    HashedSaltedPassword ByteString
      deriving Eq