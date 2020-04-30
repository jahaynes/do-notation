{-# LANGUAGE DataKinds
           , LambdaCase
           , OverloadedStrings
           , TypeOperators #-}

module Controller (runServer) where

import Requests.CreateTicket
import Requests.MoveTicket
import Storage.StorageApi
import Types.Board
import Types.Column
import Types.Ticket

import Control.Monad            (void)
import Control.Monad.IO.Class   (liftIO)
import Data.Text                (Text)
import Data.UUID                (UUID)
import Network.Wai.Handler.Warp (run)
import Servant

type DoAPI =

        "board" :> QueryParam "board" Text :> Get '[JSON] Board

   :<|> "column" :> QueryParam "columnId" UUID :> Get '[JSON] [Ticket]

   :<|> "ticket" :> "get"    :> QueryParam "columnId" UUID :> QueryParam "ticketId" UUID :> Get '[JSON] Ticket

   :<|> "ticket" :> "create" :> ReqBody '[JSON] CreateTicket :> Post '[JSON] TicketId

   :<|> "ticket" :> "move" :> ReqBody '[JSON] MoveTicket :> Post '[JSON] ()

   :<|> "static" :> Raw

server1 :: StorageApi
        -> Server DoAPI
server1 storageApi = routeQueryBoard
                :<|> routeQueryColumn
                :<|> routeQueryTicket
                :<|> routeCreateTicket
                :<|> routeMoveTicket
                :<|> serveDirectoryWebApp "frontend"

        where
        routeQueryBoard Nothing          = error "routeQueryBoard Nothing"
        routeQueryBoard (Just boardName) = liftIO $ getBoard storageApi (BoardName boardName)

        routeQueryColumn Nothing         = error "routeQueryColumn Nothing"
        routeQueryColumn (Just columnId) = liftIO $ getColumn storageApi (ColumnId columnId)

        routeQueryTicket _ Nothing             = error "routeQueryTicket columnId Nothing"
        routeQueryTicket Nothing _             = error "routeQueryTicket ticketId Nothing"
        routeQueryTicket (Just cid) (Just tid) = liftIO $ getTicket storageApi (ColumnId cid) (TicketId tid)

        routeCreateTicket (CreateTicket boardName name body) =
            liftIO $ getDefaultColumn storageApi boardName >>= \case
                Just cid -> createTicket storageApi cid name body
                Nothing  -> error "No default column"

        routeMoveTicket (MoveTicket board from to ticket) =
            liftIO . void $ moveTicket storageApi board from to ticket

doAPI :: Proxy DoAPI
doAPI = Proxy

runServer :: StorageApi
          -> IO ()
runServer storageApi =
    run 8080 . serve doAPI
             $ server1 storageApi
