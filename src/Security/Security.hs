{-# LANGUAGE BangPatterns,
             OverloadedStrings,
             TypeApplications #-}

module Security.Security (SecurityApi (..), createSecurityApi) where

import Security.AuthToken
import Types.User ( HashedSaltedPassword (..)
                  , RawPassword (..)
                  , Salt (..)
                  , UserId (..)
                  )

import Data.ByteArray     (convert)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Crypto.Hash         
import Crypto.Random      (getRandomBytes)

import           Crypto.JOSE.JWK           (JWK)
import qualified Crypto.JOSE.JWK           as JWK
import           Crypto.JOSE.JWS           (newJWSHeader, Alg(HS256))
import           Crypto.JOSE               (runJOSE)
import           Crypto.JWT                (JWTError, ClaimsSet, decodeCompact, encodeCompact, verifyJWT, defaultJWTValidationSettings)
import qualified Crypto.JWT                as JWT
import           Control.Lens              ((^.), (.~), (&), review, preview)
import           Control.Monad.Except      (ExceptT, runExceptT)
import qualified Data.ByteString.Lazy      as LBS

data SecurityApi =
    SecurityApi { signAndEncode        :: UserId -> IO AuthToken
                , authenticateJwt      :: AuthToken -> IO (Maybe UserId)
                , hashPassword         :: RawPassword -> IO (Salt, HashedSaltedPassword)
                , hashPasswordWithSalt :: Salt -> RawPassword -> HashedSaltedPassword
                }

createSecurityApi :: Text -> IO SecurityApi
createSecurityApi !jwtSecret = do

    let jwk = JWK.fromOctets (encodeUtf8 jwtSecret)

    pure $ SecurityApi
               { signAndEncode        = signAndEncodeImpl jwk
               , authenticateJwt      = verifyImpl jwk
               , hashPassword         = hashPasswordImpl
               , hashPasswordWithSalt = hashPasswordWithSaltImpl
               }

signAndEncodeImpl :: JWK -> UserId -> IO AuthToken
signAndEncodeImpl jwk (UserId userid) = do
    let claims = JWT.emptyClaimsSet & JWT.claimSub .~ preview JWT.stringOrUri userid
    res <- runJOSE @JWTError $ do
        signedJwt <- JWT.signClaims jwk (newJWSHeader ((), HS256)) claims
        pure $ encodeCompact signedJwt
    case res of
        Left _ -> error "Failed to sign JWT"
        Right lbs -> pure $ AuthToken (decodeUtf8 $ LBS.toStrict lbs)

--TODO check for some property other than just 'signed'
verifyImpl :: JWK -> AuthToken -> IO (Maybe UserId)
verifyImpl jwk (AuthToken t) = do
    res <- runExceptT @JWTError $ do
        jwt <- decodeCompact (LBS.fromStrict $ encodeUtf8 t)
        claims <- verifyJWT (defaultJWTValidationSettings (const True)) jwk jwt :: ExceptT JWTError IO ClaimsSet
        pure claims
    case res of
        Left _ -> pure Nothing
        Right claims -> 
            case claims ^. JWT.claimSub of
                Nothing -> pure Nothing
                Just sub -> pure $ Just $ UserId (review JWT.stringOrUri sub)

hashPasswordImpl :: RawPassword -> IO (Salt, HashedSaltedPassword)
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