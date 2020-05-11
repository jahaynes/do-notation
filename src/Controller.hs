{-# LANGUAGE DataKinds
           , LambdaCase
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeOperators #-}

module Controller (runServer) where

import Errors
import Routes.CreateUser
import Routes.CreateTicket
import Routes.DeleteTicket
import Routes.QueryBoard
import Routes.Login
import Routes.Logout
import Routes.MoveTicket
import Routes.QueryColumn
import Routes.QueryTicket
import Routes.UpdateTicket
import Security.Authorisation
import Security.Security
import Storage.StorageApi
import Types.Board
import Types.Ticket

import Data.Text                (Text)
import Data.UUID                (UUID)
import Network.Wai.Handler.Warp (run)
import Servant

type DoAPI =

        "login" :> ReqBody '[JSON] Login
                :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] ())

   :<|> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] ())

   :<|> "user" :> ReqBody '[JSON] CreateUser
               :> Post '[JSON] ()

   :<|> "board" :> Header "Cookie" Text
                :> QueryParam "board" BoardName
                :> Get '[JSON] (Authed Board)

   :<|> "column" :> Header "Cookie" Text
                 :> QueryParam "columnId" UUID
                 :> Get '[JSON] (Authed [Ticket])

   :<|> "ticket" :> Header "Cookie" Text
                 :> QueryParam "columnId" UUID
                 :> QueryParam "ticketId" UUID
                 :> Get '[JSON] (Authed Ticket)

   :<|> "ticket" :> Header "Cookie" Text
                 :> ReqBody '[JSON] CreateTicket
                 :> Post '[JSON] (Authed TicketId)

   :<|> "ticket" :> Header "Cookie" Text
                 :> ReqBody '[JSON] UpdateTicket
                 :> Put '[JSON] (Authed ())

   :<|> "ticket" :> Header "Cookie" Text
                 :> ReqBody '[JSON] DeleteTicket
                 :> Delete '[JSON] (Authed ())

   :<|> "ticket" :> "move" :> Header "Cookie" Text
                           :> ReqBody '[JSON] MoveTicket
                           :> Post '[JSON] (Authed ())

   :<|> "static" :> Raw

server :: SecurityApi IO -> StorageApi -> Server DoAPI
server securityApi storageApi =

         -- TODO handlers up here
         routeLogin securityApi storageApi

    :<|> routeLogout securityApi storageApi

         -- TODO rename routeSignup ?
    :<|> routeCreateUser securityApi storageApi

    :<|> (\mCookie mBoard -> handle
                           $ withAuthorisation securityApi mCookie
                           $ routeQueryBoard storageApi mBoard
                           )

    :<|> (\mCookie mUUID -> handle
                          $ withAuthorisation securityApi mCookie
                          $ routeQueryColumn storageApi mUUID
                          )

    :<|> (\mCookie mColumnId mTicketId -> handle 
                                        $ withAuthorisation securityApi mCookie
                                        $ routeQueryTicket storageApi mColumnId mTicketId
                                        )

    :<|> (\mCookie createTicketReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ routeCreateTicket storageApi createTicketReq
                                    )

    :<|> (\mCookie updateTicketReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ routeUpdateTicket storageApi updateTicketReq
                                    )

    :<|> (\mCookie deleteTicketReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ routeDeleteTicket storageApi deleteTicketReq
                                    )

    :<|> (\mCookie moveTicketReq -> handle
                                  $ withAuthorisation securityApi mCookie
                                  $ routeMoveTicket storageApi moveTicketReq
                                  )

    :<|> serveDirectoryWebApp "frontend"

runServer :: SecurityApi IO -> StorageApi -> IO ()
runServer securityApi storageApi =
    run 8080 . serve doAPI $ server securityApi storageApi

    where
    doAPI :: Proxy DoAPI
    doAPI = Proxy