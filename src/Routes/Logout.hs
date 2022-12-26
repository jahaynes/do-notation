{-# LANGUAGE DataKinds, 
             OverloadedStrings #-}

module Routes.Logout (routeLogout) where

import Security.Authorisation

import Data.Binary.Builder  (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding   (decodeUtf8)
import Web.Cookie

routeLogout :: CookieHeader
routeLogout = CookieHeader
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
