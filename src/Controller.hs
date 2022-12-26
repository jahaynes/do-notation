{-# LANGUAGE DataKinds
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeOperators #-}

module Controller (runServer) where

import Errors
import Routes.CreateBoard
import Routes.CreateColumn
import Routes.CreateTicket
import Routes.DeleteBoard
import Routes.DeleteTicket
import Routes.Health
import Routes.QueryBoard
import Routes.ShareBoard
import Routes.Login
import Routes.Logout
import Routes.MoveTicket
import Routes.QueryColumn
import Routes.QueryTicket
import Routes.Signup
import Routes.UpdateTicket
import Security.Authorisation
import Security.Security
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Column
import Types.Ticket

import Control.Monad.IO.Class   (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant

type DoAPI =

        "health" :> Get '[JSON] Health

   :<|> "login" :> ReqBody '[JSON] Login
                :> Post '[JSON] (Headers '[Header "Set-Cookie" CookieHeader] ())

   :<|> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" CookieHeader] ())

   :<|> "signup" :> ReqBody '[JSON] Signup
                 :> Post '[JSON] () --TODO could Set-Cookie here to get logged in

   :<|> "boards" :> Header "Cookie" CookieHeader
                 :> Get '[JSON] (Authed [(BoardId, BoardName)])

   :<|> "board" :> Header "Cookie" CookieHeader
                :> QueryParam "board" BoardId
                :> Get '[JSON] (Authed Board)

   :<|> "board" :> Header "Cookie" CookieHeader
                :> ReqBody '[JSON] CreateBoard
                :> Post '[JSON] (Authed BoardId)

   :<|> "board" :> Header "Cookie" CookieHeader
                :> ReqBody '[JSON] DeleteBoard
                :> Delete '[JSON] (Authed ())
 
   :<|> "share-board" :> Header "Cookie" CookieHeader
                      :> ReqBody '[JSON] ShareBoard
                      :> Post '[JSON] (Authed ())

   :<|> "column" :> Header "Cookie" CookieHeader
                 :> QueryParam "columnId" ColumnId
                 :> Get '[JSON] (Authed [Ticket])

   :<|> "column" :> Header "Cookie" CookieHeader
                 :> ReqBody '[JSON] CreateColumn
                 :> Post '[JSON] (Authed ColumnId)

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

   :<|> Get '[JSON] ()

   :<|> Raw

server :: HealthApi -> SecurityApi IO -> StorageApi -> Server DoAPI
server healthApi securityApi storageApi =

         liftIO (getHealth healthApi)

    :<|> (\login -> handle 
                  $ routeLogin securityApi storageApi login >>= \cookieHeader -> pure $ addHeader cookieHeader ()
                  )

    :<|> pure (addHeader routeLogout ())

    :<|> (handle . routeSignup securityApi storageApi)

    :<|> (\mCookie -> handle
                    $ withAuthorisation securityApi mCookie
                    $ routeQueryBoards storageApi
                    )

    :<|> (\mCookie mBoard -> handle
                           $ withAuthorisation securityApi mCookie
                           $ \_ -> routeQueryBoard storageApi mBoard
                           )

    :<|> (\mCookie createBoardReq -> handle
                                   $ withAuthorisation securityApi mCookie
                                   $ routeCreateBoard storageApi createBoardReq
                                   )

    :<|> (\mCookie deleteBoardReq -> handle
                                   $ withAuthorisation securityApi mCookie
                                   $ \_ -> routeDeleteBoard storageApi deleteBoardReq)

    :<|> (\mCookie shareBoardReq -> handle
                                  $ withAuthorisation securityApi mCookie
                                  $ \userId -> routeShareBoard storageApi shareBoardReq userId)

    :<|> (\mCookie mUUID -> handle
                          $ withAuthorisation securityApi mCookie
                          $ \_ -> routeQueryColumn storageApi mUUID
                          )

    :<|> (\mCookie createColumnReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ \_ -> routeCreateColumn storageApi createColumnReq
                                    )

    :<|> (\mCookie mColumnId mTicketId -> handle
                                        $ withAuthorisation securityApi mCookie
                                        $ \_ -> routeQueryTicket storageApi mColumnId mTicketId
                                        )

    :<|> (\mCookie createTicketReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ \_ -> routeCreateTicket storageApi createTicketReq
                                    )

    :<|> (\mCookie updateTicketReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ \_ -> routeUpdateTicket storageApi updateTicketReq
                                    )

    :<|> (\mCookie deleteTicketReq -> handle
                                    $ withAuthorisation securityApi mCookie
                                    $ \_ -> routeDeleteTicket storageApi deleteTicketReq
                                    )

    :<|> (\mCookie moveTicketReq -> handle
                                  $ withAuthorisation securityApi mCookie
                                  $ \_ -> routeMoveTicket storageApi moveTicketReq
                                  )

    :<|> (throwError $ err301 {errHeaders = [("Location", "/index.html")]})

    :<|> serveDirectoryWebApp "frontend"

runServer :: HealthApi -> Int -> SecurityApi IO -> StorageApi -> IO ()
runServer healthApi port securityApi storageApi =
    run port . serve doAPI $ server healthApi securityApi storageApi

    where
    doAPI :: Proxy DoAPI
    doAPI = Proxy
