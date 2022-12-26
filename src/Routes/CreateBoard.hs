{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Routes.CreateBoard (CreateBoard, routeCreateBoard) where

import Errors
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Json
import Types.User

import           Control.Monad              (when)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics               (Generic)

newtype CreateBoard =
    CreateBoard { cb_boardName :: BoardName
                } deriving Generic

instance FromJSON CreateBoard where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeCreateBoard :: StorageApi
                 -> CreateBoard
                 -> UserId
                 -> ExceptT ErrorResponse IO BoardId
routeCreateBoard storageApi (CreateBoard boardName) userId = do

    validate boardName

    boardNotAlreadyExists

    catchAll "Could not create board." $
        lift $ do
            boardId <- createBoard storageApi boardName
            createUser storageApi userId boardId
            pure boardId

    where
    boardNotAlreadyExists :: ExceptT ErrorResponse IO ()
    boardNotAlreadyExists = do
        boardNames <- map snd <$> lift (getBoards storageApi userId)
        when (boardName `elem` boardNames) $
            err 403 "Board already exists"

    validate :: BoardName -> ExceptT ErrorResponse IO ()
    validate (BoardName bn)
        | T.null bn        = err 400 "Board name was empty"
        | T.length bn > 40 = err 400 "Board name too long (40+)"
        | otherwise        = pure ()