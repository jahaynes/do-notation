{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryColumn where

import Errors
import Storage.StorageApi
import Types.Column
import Types.Ticket

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT)

routeQueryColumn :: StorageApi
                 -> Maybe ColumnId
                 -> ExceptT ErrorResponse IO [Ticket]
routeQueryColumn storageApi mColumnId =

    catchAll "Could not query column." $ do
        columnId <- getColumnId mColumnId
        lift (getColumn storageApi columnId)

    where
    getColumnId Nothing   = err 400 "No columnId supplied."
    getColumnId (Just ci) = pure ci