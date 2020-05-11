{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryColumn where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Ticket

import Data.UUID                (UUID)

routeQueryColumn :: StorageApi
                 -> Maybe UUID
                 -> IO (Either ErrorResponse [Ticket])
routeQueryColumn storageApi mColumnId =
    case mColumnId of
        Just columnId -> Right <$> getColumn storageApi (ColumnId columnId)
        Nothing       -> errorResponse 400 "coludmId not supplied."