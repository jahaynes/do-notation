{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryBoard where

import Errors
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.User

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)

routeQueryBoards :: StorageApi
                 -> UserId
                 -> ExceptT ErrorResponse IO [(BoardId, BoardName)]
routeQueryBoards storageApi userId = 
    catchAll "Could not query boards." 
           $ lift (getBoards storageApi userId)

routeQueryBoard :: StorageApi
                -> Maybe BoardId
                -> ExceptT ErrorResponse IO Board
routeQueryBoard storageApi mBoardId = 

    catchAll "Could not query board." $ do
        boardId <- getBoardId mBoardId
        mBoard  <- lift (getBoard storageApi boardId)
        case mBoard of
            Just board -> pure board
            Nothing    -> err 404 "Board not found."

    where
    getBoardId Nothing   = err 400 "No board queried."
    getBoardId (Just bi) = pure bi
