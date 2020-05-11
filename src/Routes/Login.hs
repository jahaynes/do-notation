{-# LANGUAGE DataKinds,
             OverloadedStrings #-}

module Routes.Login where

import Requests.Login
import Routes.Shared
import Security.AuthToken
import Security.Security
import Storage.StorageApi

import Control.Monad.IO.Class   (liftIO)
import Data.Binary.Builder
import Data.ByteString.Lazy     (toStrict)
import Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import Data.Text                (Text)
import Servant
import Web.Cookie

routeLogin :: SecurityApi m
           -> StorageApi
           -> Login
           -> Handler (Headers '[Header "Set-Cookie" Text] ())
routeLogin securityApi storageApi (Login uname pw) = do
    maybeSaltPw <- liftIO $ getSaltAndPassword storageApi uname
    case maybeSaltPw of
        Nothing -> err 401 "Bad username or password."
        Just (salt, storedPw) ->
            let checkedPw = hashPasswordWithSalt securityApi salt pw
            in if checkedPw == storedPw
                   then let (AuthToken authToken) = signAndEncode securityApi mempty
                            cookie = decodeUtf8
                                   . toStrict
                                   . toLazyByteString
                                   . renderSetCookie 
                                   $ defaultSetCookie { setCookieName     = "authToken"
                                                      , setCookieHttpOnly = True
                                                      , setCookieSecure   = True
                                                      , setCookiePath     = Just "/"
                                                      , setCookieSameSite = Just sameSiteStrict
                                                      , setCookieValue    = encodeUtf8 authToken }
                        in pure . addHeader cookie $ ()
                else err 401 "Bad username or password."
