{-# LANGUAGE OverloadedStrings #-}

module Security.Authorisation ( Authed
                              , withAuthorisation
                              ) where

import Errors
import Security.AuthToken
import Security.Security

import Control.Monad.Trans.Except
import Data.Aeson                 (ToJSON (..))
import Data.ByteString.Lazy       (ByteString)
import Data.Text                  (Text)
import Data.Text.Encoding         (encodeUtf8, decodeUtf8')
import Web.Cookie

newtype Authed a = Authed a

instance ToJSON a => ToJSON (Authed a) where
    toJSON (Authed a) = toJSON a

withAuthorisation :: Monad m => SecurityApi n
                             -> Maybe Text        -- TODO Cookie?
                             -> ExceptT ErrorResponse m a
                             -> ExceptT ErrorResponse m (Authed a)
withAuthorisation securityApi mCookieTxt handler = do

    cookieTxt <- case mCookieTxt of
                     Nothing        -> err' 401 "No authorisation supplied."
                     Just cookieTxt -> pure cookieTxt

    authToken <- case getAuthTokenCookie cookieTxt of
                     Left e -> let msg = "Could not parse auth: " <> e
                               in err' 400 msg
                     Right r -> pure r

    if verify securityApi authToken
        then Authed <$> handler
        else err' 401 "Auth token rejected."

getAuthTokenCookie :: Text -> Either ByteString AuthToken
getAuthTokenCookie cookieTxt = do

    let cookies = parseCookies $ encodeUtf8 cookieTxt

    val <- case filter (\(k,_) -> k == "authToken") cookies of
               []       -> Left "No key 'authToken' in cookies"
               [(_, v)] -> Right v
               _        -> Left "Too many 'authToken' keys in cookies"

    case decodeUtf8' val of
        Left _  -> Left "Invalid utf8 in authToken value"
        Right x -> Right (AuthToken x)
