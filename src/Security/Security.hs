{-# LANGUAGE OverloadedStrings #-}

module Security.Security where

import Security.AuthToken
import Types.User ( HashedSaltedPassword (..)
                  , RawPassword (..)
                  , Salt (..)
                  )

import Data.ByteArray     (convert)
import Data.Maybe         (isJust)
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash         
import Crypto.Random      (MonadRandom, getRandomBytes)

import           Web.JWT      (JWTClaimsSet, Signer)
import qualified Web.JWT as J

data SecurityApi m =
    SecurityApi { signAndEncode        :: JWTClaimsSet -> AuthToken
                , verify               :: AuthToken -> Bool
                , hashPassword         :: RawPassword -> m (Salt, HashedSaltedPassword)
                , hashPasswordWithSalt :: Salt -> RawPassword -> HashedSaltedPassword
                }

createSecurityApi :: MonadRandom m => m (SecurityApi m)
createSecurityApi = do

    let signer = J.hmacSecret "I'm a secret!"

    pure $ SecurityApi
               { signAndEncode        = AuthToken <$> J.encodeSigned signer mempty
               , verify               = verifyImpl signer
               , hashPassword         = hashPasswordImpl
               , hashPasswordWithSalt = hashPasswordWithSaltImpl
               }

--TODO check for some property other than just 'signed'
verifyImpl :: Signer -> AuthToken -> Bool
verifyImpl signer (AuthToken t) =
    isJust (J.decodeAndVerifySignature signer t) 

hashPasswordImpl :: MonadRandom m => RawPassword -> m (Salt, HashedSaltedPassword)
hashPasswordImpl pword = do
    salt <- Salt <$> getRandomBytes 32
    pure (salt, hashPasswordWithSaltImpl salt pword)

hashPasswordWithSaltImpl :: Salt -> RawPassword -> HashedSaltedPassword
hashPasswordWithSaltImpl (Salt salt) (RawPassword pword) =
    HashedSaltedPassword . convert 
                         . hashFinalize
                         . (`hashUpdate` (encodeUtf8 pword))
                         . (`hashUpdate` salt)
                         $ hashInitWith SHA3_512
