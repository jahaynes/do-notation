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

   :<|> "ticket" :> "create" :> ReqBody '[JSON] CreateTicket :> Post '[JSON] TicketId

   :<|> "ticket" :> "move" :> ReqBody '[JSON] MoveTicket :> Post '[JSON] ()

   :<|> "static" :> Raw

server1 :: StorageApi
        -> Server DoAPI
server1 storageApi =

          liftIO . routeQueryBoard

     :<|> liftIO . routeQueryColumn

     :<|> liftIO . routeCreateTicket

     :<|> liftIO . routeMoveTicket

     :<|> serveDirectoryWebApp "frontend"

        where
        routeQueryBoard Nothing          = error "routeQueryBoard Nothing"
        routeQueryBoard (Just boardName) = getBoard storageApi (BoardName boardName)

        routeQueryColumn Nothing         = error "routeQueryColumn Nothing"
        routeQueryColumn (Just columnId) = getColumn storageApi (ColumnId columnId)

        routeCreateTicket (CreateTicket boardName name body) =
            getDefaultColumn storageApi boardName >>= \case
                Just cid -> createTicket storageApi cid name body
                Nothing  -> error "No default column"

        routeMoveTicket (MoveTicket board from to ticket) =
            void $ moveTicket storageApi board from to ticket

doAPI :: Proxy DoAPI
doAPI = Proxy

runServer :: StorageApi
          -> IO ()
runServer storageApi =
    run 8080 . serve doAPI
             $ server1 storageApi
