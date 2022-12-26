module Security.AuthToken (AuthToken (..)) where

import Data.Aeson (ToJSON (..))
import Data.Text  (Text)
import Servant    (FromHttpApiData (..))

newtype AuthToken =
    AuthToken { at_value :: Text
              }

instance FromHttpApiData AuthToken where
    parseUrlPiece txt = AuthToken <$> parseUrlPiece txt

instance ToJSON AuthToken where
    toJSON (AuthToken t) = toJSON t