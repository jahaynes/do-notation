{-# LANGUAGE DataKinds,
             DeriveGeneric,
             OverloadedStrings #-}

module Routes.Login where

import Security.Authorisation
import Errors
import Security.AuthToken
import Security.Security
import Storage.StorageApi
import Types.Json         (chop)
import Types.User         (UserId, RawPassword)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Binary.Builder
import Data.ByteString.Lazy   (toStrict)
import Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import GHC.Generics           (Generic)
import Servant
import Web.Cookie

data Login =
    Login { l_username    :: !UserId
          , l_rawpassword :: !RawPassword
          } deriving Generic

instance FromJSON Login where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeLogin :: SecurityApi m
           -> StorageApi
           -> Login
           -> Handler (Headers '[Header "Set-Cookie" CookieHeader] ())
routeLogin securityApi storageApi (Login uname pw) = do
    maybeSaltPw <- liftIO $ getSaltAndPassword storageApi uname
    case maybeSaltPw of
        Nothing -> err 401 "Bad username or password."
        Just (salt, storedPw) ->
            let checkedPw = hashPasswordWithSalt securityApi salt pw
            in if checkedPw == storedPw
                   then let (AuthToken authToken) = signAndEncode securityApi mempty
                            cookie = CookieHeader
                                   . decodeUtf8
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
