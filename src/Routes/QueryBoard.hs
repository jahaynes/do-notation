{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryBoard where

import Errors
import Storage.StorageApi
import Types.Board

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)

routeQueryBoard :: StorageApi
                -> Maybe BoardName
                -> ExceptT ErrorResponse IO Board
routeQueryBoard storageApi mBoardName =

    catchAll "Could not query board." $ do
        boardName <- getBoardName mBoardName
        lift (getBoard storageApi boardName)

    where
    getBoardName Nothing   = err' 400 "No board queried."
    getBoardName (Just bn) = pure bn
