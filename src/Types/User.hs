module Types.User ( HashedSaltedPassword (..)
                  , RawPassword (..)
                  , Salt (..)
                  , UserId (..)
                  ) where

import Data.Aeson      (FromJSON, parseJSON)
import Data.ByteString (ByteString)
import Data.Text       (Text)

newtype UserId =
    UserId Text

instance FromJSON UserId where
    parseJSON u = UserId <$> parseJSON u

newtype RawPassword =
    RawPassword Text

instance FromJSON RawPassword where
    parseJSON p = RawPassword <$> parseJSON p

newtype Salt =
    Salt ByteString

newtype HashedSaltedPassword = 
    HashedSaltedPassword ByteString
      deriving Eq