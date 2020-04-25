{-# LANGUAGE DataKinds
           , OverloadedStrings
           , TypeOperators #-}

module Controller (runServer) where

import Storage.StorageApi
import Types.Board
import Types.Column
import Types.CreateTicket
import Types.Ticket

import Control.Monad.IO.Class   (liftIO)
import Data.Text                (Text)
import Data.UUID                (UUID)
import Network.Wai.Handler.Warp (run)
import Servant

type DoAPI =

        "board" :> QueryParam "board" Text :> Get '[JSON] Board

   :<|> "column" :> QueryParam "columnId" UUID :> Get '[JSON] [Ticket]

   :<|> "ticket" :> "create" :> ReqBody '[JSON] CreateTicket :> Post '[JSON] TicketId

   :<|> "static" :> Raw

server1 :: StorageApi
        -> Server DoAPI
server1 storageApi =

          liftIO . routeQueryBoard

     :<|> liftIO . routeQueryColumn

     :<|> liftIO . routeCreateTicket

     :<|> serveDirectoryWebApp "frontend"

        where
        routeQueryBoard Nothing          = error "routeQueryBoard Nothing"
        routeQueryBoard (Just boardName) = getBoard storageApi (BoardId boardName)

        routeQueryColumn Nothing         = error "routeQueryColumn Nothing"
        routeQueryColumn (Just columnId) = getColumn storageApi (ColumnId columnId)

        routeCreateTicket (CreateTicket boardName name body) = do
            Just cid <- getDefaultColumn storageApi boardName
            createTicket storageApi cid name body

doAPI :: Proxy DoAPI
doAPI = Proxy

runServer :: StorageApi
          -> IO ()
runServer storageApi =
    run 8080 . serve doAPI
             $ server1 storageApi
