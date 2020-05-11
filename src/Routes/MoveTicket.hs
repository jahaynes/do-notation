module Routes.MoveTicket where

import Requests.MoveTicket
import Storage.StorageApi

import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)
import Servant                (Handler)

routeMoveTicket :: StorageApi -> MoveTicket -> Handler ()
routeMoveTicket storageApi (MoveTicket board from to ticket) =
    liftIO . void $ moveTicket storageApi board from to ticket