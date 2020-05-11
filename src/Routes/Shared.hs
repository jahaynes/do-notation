{-# LANGUAGE OverloadedStrings #-}

module Routes.Shared ( Authed
                     , err
                     , withAuthorisation
                     ) where

import Security.AuthToken
import Security.Security

import Data.Aeson             (ToJSON (..))
import Data.ByteString.Lazy   (ByteString)
import Data.Text              (Text)
import Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import Safe                   (headMay)
import Servant                (Handler, ServerError (..), throwError)
import Web.Cookie

-- TODO rename to authorisation ?

newtype Authed a = Authed a

instance ToJSON a => ToJSON (Authed a) where
    toJSON (Authed a) = toJSON a

withAuthorisation :: SecurityApi m
                  -> Maybe Text
                  -> Handler a
                  -> Handler (Authed a)
withAuthorisation           _       Nothing  _ = err 401 "No authorisation supplied."
withAuthorisation securityApi (Just cookie) handler =
    case getAuthTokenCookie cookie of
        Nothing -> err 401 "Could not parse authorisation."
        Just authToken ->
            if verify securityApi authToken
                then Authed <$> handler
                else err 401 "Authorisation failed."

err :: Int -> ByteString -> Handler a
err code msg = throwError $ ServerError { errHTTPCode     = code
                                        , errReasonPhrase = ""
                                        , errBody         = msg 
                                        , errHeaders      = []
                                        }

-- TODO rewrite eitherT ?
getAuthTokenCookie :: Text -> Maybe AuthToken
getAuthTokenCookie cookieTxt =
    AuthToken <$> ( headMay
                    . map (decodeUtf8 . snd)
                    . filter (\(k,_) -> k == "authToken")
                    . parseCookies
                    . encodeUtf8
                    $ cookieTxt )