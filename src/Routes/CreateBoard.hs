{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Routes.CreateBoard where

import Errors
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Column
import Types.Json
import Types.User

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import GHC.Generics               (Generic)

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
routeCreateBoard storageApi (CreateBoard boardName) userId =
    catchAll "Could not create board." $
        lift $ do
            -- TODO maybe boardid - don't create board if it already exists
            boardId <- createBoard storageApi boardName
            createUser storageApi userId boardId
            _ <- createColumn storageApi boardId (ColumnPosition 1) (ColumnName "default")
            pure boardId
