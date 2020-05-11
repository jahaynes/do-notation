{-# LANGUAGE OverloadedStrings #-}

module Security.Authorisation ( Authed
                              , withAuthorisation
                              ) where

import Errors
import Security.AuthToken
import Security.Security

import Data.Aeson         (ToJSON (..))
import Data.Text          (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Safe               (headMay)
import Web.Cookie

newtype Authed a = Authed a

instance ToJSON a => ToJSON (Authed a) where
    toJSON (Authed a) = toJSON a

-- TODO remove handler

withAuthorisation :: SecurityApi m
                  -> Maybe Text
                  -> IO (Either ErrorResponse         a)
                  -> IO (Either ErrorResponse (Authed a))
withAuthorisation           _       Nothing       _ = errorResponse 401 "No authorisation supplied."
withAuthorisation securityApi (Just cookie) handler =
    case getAuthTokenCookie cookie of
        Nothing -> errorResponse 401 "Could not parse authorisation."
        Just authToken ->
            if verify securityApi authToken
                then fmap Authed <$> handler
                else errorResponse 401 "Authorisation failed."

-- TODO rewrite eitherT ?
getAuthTokenCookie :: Text -> Maybe AuthToken
getAuthTokenCookie cookieTxt =
    AuthToken <$> ( headMay
                    . map (decodeUtf8 . snd)
                    . filter (\(k,_) -> k == "authToken")
                    . parseCookies
                    . encodeUtf8
                    $ cookieTxt )