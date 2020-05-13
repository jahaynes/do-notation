{-# LANGUAGE DataKinds, 
             OverloadedStrings #-}

module Routes.Logout where

import Security.Authorisation
import Security.Security
import Storage.StorageApi

import Data.Binary.Builder  (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding   (decodeUtf8)
import Servant
import Web.Cookie

routeLogout :: SecurityApi m
            -> StorageApi
            -> Handler (Headers '[Header "Set-Cookie" CookieHeader] ())
routeLogout _ _ = do

    let cookie = CookieHeader
               . decodeUtf8
               . toStrict
               . toLazyByteString
               . renderSetCookie 
               $ defaultSetCookie { setCookieName     = "authToken"
                                  , setCookieHttpOnly = True
                                  , setCookieSecure   = True
                                  , setCookiePath     = Just "/"
                                  , setCookieSameSite = Just sameSiteStrict
                                  , setCookieValue    = "" }
    pure . addHeader cookie $ ()
