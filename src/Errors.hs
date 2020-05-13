{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Errors ( ErrorResponse
              , catchAll
              , handle
              , err'
              , err
              ) where

import Control.DeepSeq            (NFData)
import Control.Exception.Safe     (MonadCatch, catchAnyDeep)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString.Lazy       (ByteString)
import Servant                    (Handler, ServerError (..), throwError)

data ErrorResponse =
    ErrorResponse !Int !ByteString

--TODO remove
err :: Int -> ByteString -> Handler a
err code msg = throwError $ ServerError { errHTTPCode     = code
                                        , errReasonPhrase = ""
                                        , errBody         = msg
                                        , errHeaders      = []
                                        }

err' :: Monad m => Int -> ByteString -> ExceptT ErrorResponse m a
err' code msg = throwE $ ErrorResponse code msg

catchAll :: (MonadCatch m, MonadIO m, NFData a)
         => ByteString
         -> ExceptT ErrorResponse m a
         -> ExceptT ErrorResponse m a
catchAll errMsg ma =
    catchAnyDeep ma
                 (\_ -> throwE $ ErrorResponse 500 errMsg)

handle :: ExceptT ErrorResponse IO a -> Handler a
handle ioa = liftIO (runExceptT ioa) >>= \case
    Right r -> pure r
    Left (ErrorResponse code msg) ->
        throwError $ ServerError { errHTTPCode     = code
                                 , errReasonPhrase = ""
                                 , errBody         = msg 
                                 , errHeaders      = []
                                 }