{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Errors ( ErrorResponse
              , catchAll
              , handle
              , err
              , err'
              ) where

import Control.DeepSeq                  (NFData)
import Control.Exception.Safe           (MonadCatch, catchAnyDeep)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 as L8 (ByteString, putStrLn)
import Servant                          (Handler, ServerError (..), throwError)

data ErrorResponse =
    ErrorResponse !Int !ByteString

err :: Monad m => Int -> ByteString -> ExceptT ErrorResponse m a
err code msg = throwE $ ErrorResponse code msg

err' :: Applicative m => Int -> ByteString -> m (Either ErrorResponse a)
err' code msg = pure . Left $ ErrorResponse code msg

catchAll :: (MonadCatch m, MonadIO m, NFData a)
         => ByteString
         -> ExceptT ErrorResponse m a
         -> ExceptT ErrorResponse m a
catchAll errMsg ma =
    catchAnyDeep ma
                 (\ex -> do liftIO $ do L8.putStrLn errMsg
                                        print ex
                            throwE $ ErrorResponse 500 errMsg)

handle :: ExceptT ErrorResponse IO a -> Handler a
handle ioa = liftIO (runExceptT ioa) >>= \case
    Right r -> pure r
    Left (ErrorResponse code msg) ->
        throwError $ ServerError { errHTTPCode     = code
                                 , errReasonPhrase = ""
                                 , errBody         = msg 
                                 , errHeaders      = []
                                 }
