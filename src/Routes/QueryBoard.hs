{-# LANGUAGE OverloadedStrings #-}

module Routes.QueryBoard where

import Routes.Shared
import Storage.StorageApi
import Types.Board              (Board, BoardName)

import Control.Monad.IO.Class   (liftIO)
import Servant                  (Handler)

routeQueryBoard :: StorageApi -> Maybe BoardName -> Handler Board
routeQueryBoard          _          Nothing = err 400 "No board queried."
routeQueryBoard storageApi (Just boardName) = liftIO $ getBoard storageApi boardName