{-# LANGUAGE BangPatterns,
             OverloadedStrings #-}

module Security.Security where

import Security.AuthToken
import Types.User ( HashedSaltedPassword (..)
                  , RawPassword (..)
                  , Salt (..)
                  , UserId (..)
                  )

import Data.ByteArray     (convert)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash         
import Crypto.Random      (MonadRandom, getRandomBytes)

import           Web.JWT      (EncodeSigner, VerifySigner)
import qualified Web.JWT as J

data SecurityApi m =
    SecurityApi { signAndEncode        :: UserId -> AuthToken
                , authenticateJwt      :: AuthToken -> Maybe UserId
                , hashPassword         :: RawPassword -> m (Salt, HashedSaltedPassword)
                , hashPasswordWithSalt :: Salt -> RawPassword -> HashedSaltedPassword
                }

createSecurityApi :: MonadRandom m => Text -> m (SecurityApi m)
createSecurityApi !jwtSecret = do

    let encodeSigner = J.hmacSecret jwtSecret
        verifySigner = J.toVerify encodeSigner

    pure $ SecurityApi
               { signAndEncode        = signAndEncodeImpl encodeSigner
               , authenticateJwt      = verifyImpl verifySigner
               , hashPassword         = hashPasswordImpl
               , hashPasswordWithSalt = hashPasswordWithSaltImpl
               }

signAndEncodeImpl :: EncodeSigner -> UserId -> AuthToken
signAndEncodeImpl signer (UserId userid) =    
    AuthToken . J.encodeSigned signer mempty
              $ mempty { J.sub = J.stringOrURI userid }

--TODO check for some property other than just 'signed'
verifyImpl :: VerifySigner -> AuthToken -> Maybe UserId
verifyImpl signer (AuthToken t) = do
    token <- J.decodeAndVerifySignature signer t
    uid   <- J.sub . J.claims $ token
    pure . UserId $ J.stringOrURIToText uid

hashPasswordImpl :: MonadRandom m => RawPassword -> m (Salt, HashedSaltedPassword)
hashPasswordImpl pword = do
    salt <- Salt <$> getRandomBytes 32
    pure (salt, hashPasswordWithSaltImpl salt pword)

hashPasswordWithSaltImpl :: Salt -> RawPassword -> HashedSaltedPassword
hashPasswordWithSaltImpl (Salt salt) (RawPassword pword) =
    HashedSaltedPassword . convert 
                         . hashFinalize
                         . (`hashUpdate` encodeUtf8 pword)
                         . (`hashUpdate` salt)
                         $ hashInitWith SHA3_512
