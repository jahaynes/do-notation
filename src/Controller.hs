{-# LANGUAGE DataKinds
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
import Types.Column
import Types.Ticket

import Network.Wai.Handler.Warp (run)
import Servant

type DoAPI =

        "login" :> ReqBody '[JSON] Login
                :> Post '[JSON] (Headers '[Header "Set-Cookie" CookieHeader] ())

   :<|> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" CookieHeader] ())

   :<|> "user" :> ReqBody '[JSON] CreateUser
               :> Post '[JSON] ()

   :<|> "board" :> Header "Cookie" CookieHeader
                :> QueryParam "board" BoardName
                :> Get '[JSON] (Authed Board)

   :<|> "column" :> Header "Cookie" CookieHeader
                 :> QueryParam "columnId" ColumnId
                 :> Get '[JSON] (Authed [Ticket])

   :<|> "ticket" :> Header "Cookie" CookieHeader
                 :> QueryParam "columnId" ColumnId
                 :> QueryParam "ticketId" TicketId
                 :> Get '[JSON] (Authed Ticket)

   :<|> "ticket" :> Header "Cookie" CookieHeader
                 :> ReqBody '[JSON] CreateTicket
                 :> Post '[JSON] (Authed TicketId)

   :<|> "ticket" :> Header "Cookie" CookieHeader
                 :> ReqBody '[JSON] UpdateTicket
                 :> Put '[JSON] (Authed ())

   :<|> "ticket" :> Header "Cookie" CookieHeader
                 :> ReqBody '[JSON] DeleteTicket
                 :> Delete '[JSON] (Authed ())

   :<|> "ticket" :> "move" :> Header "Cookie" CookieHeader
                           :> ReqBody '[JSON] MoveTicket
                           :> Post '[JSON] (Authed ())

   :<|> "static" :> Raw

server :: SecurityApi IO -> StorageApi -> Server DoAPI
server securityApi storageApi =

         (\login -> handle 
                  $ routeLogin' securityApi storageApi login >>= \cookieHeader -> pure $ addHeader cookieHeader ()
                  )

    :<|> pure (addHeader routeLogout ())

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