{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryColumn where

import Routes.Shared
import Storage.StorageApi
import Types.Column
import Types.Ticket

import Control.Monad.IO.Class   (liftIO)
import Data.UUID                (UUID)
import Servant

routeQueryColumn :: StorageApi
                 -> Maybe UUID
                 -> Handler [Ticket]
routeQueryColumn          _         Nothing = err 400 "coludmId not supplied."
routeQueryColumn storageApi (Just columnId) = liftIO $ getColumn storageApi (ColumnId columnId)