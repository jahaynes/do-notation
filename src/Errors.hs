{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Errors ( ErrorResponse
              , errorResponse
              , err
              , handle
              ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy   (ByteString)
import Servant                (Handler, ServerError (..), throwError)

data ErrorResponse =
    ErrorResponse !Int !ByteString

errorResponse :: Applicative m => Int -> ByteString -> m (Either ErrorResponse a)
errorResponse code msg = pure . Left $ ErrorResponse code msg

--TODO remove
err :: Int -> ByteString -> Handler a
err code msg = throwError $ ServerError { errHTTPCode     = code
                                        , errReasonPhrase = ""
                                        , errBody         = msg 
                                        , errHeaders      = []
                                        }

handle :: IO (Either ErrorResponse a) -> Handler a
handle ioa = liftIO ioa >>= \case
    Right r -> pure r
    Left (ErrorResponse code msg) ->
        throwError $ ServerError { errHTTPCode     = code
                                 , errReasonPhrase = ""
                                 , errBody         = msg 
                                 , errHeaders      = []
                                 }