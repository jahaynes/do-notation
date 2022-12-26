{-# LANGUAGE DeriveGeneric,
             LambdaCase,
             OverloadedStrings #-}

module Routes.DeleteBoard (DeleteBoard, routeDeleteBoard) where

import Errors
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Column
import Types.Json

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson
import qualified Data.Vector as V
import           GHC.Generics               (Generic)

newtype DeleteBoard =
    DeleteBoard { db_boardId :: BoardId
                } deriving Generic

instance FromJSON DeleteBoard where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeDeleteBoard :: StorageApi
                 -> DeleteBoard
                 -> ExceptT ErrorResponse IO ()
routeDeleteBoard storageApi (DeleteBoard boardId) =

    catchAll "Could not delete board." $

        hasNoTickets >>= \case

            True -> lift $ deleteBoard storageApi boardId

            False -> err 403 "Cannot delete a board while it has tickets"

    where
    hasNoTickets :: ExceptT ErrorResponse IO Bool
    hasNoTickets = do

        mBoard <- lift $ getBoard storageApi boardId

        case mBoard of

            Nothing -> err 404 "Board not found"

            Just board -> lift $ do

                -- TODO - can abort early if ticket found
                ticketss <- V.mapM (getColumnTickets storageApi)
                          . V.map c_columnid
                          . b_columns 
                          $ board

                pure . V.all null $ ticketss
