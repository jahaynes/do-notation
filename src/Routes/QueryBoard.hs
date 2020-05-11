{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryBoard where

import Errors
import Storage.StorageApi
import Types.Board

routeQueryBoard :: StorageApi
                -> Maybe BoardName
                -> IO (Either ErrorResponse Board)
routeQueryBoard storageApi mBoardName =
    case mBoardName of
        Just boardName -> Right <$> getBoard storageApi boardName
        Nothing        -> errorResponse 400 "No board queried."