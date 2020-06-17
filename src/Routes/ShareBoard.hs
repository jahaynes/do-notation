{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Routes.ShareBoard where

import Errors
import Storage.StorageApi
import Types.BoardId
import Types.Json
import Types.User

import           Control.Monad              (unless)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson
import           GHC.Generics               (Generic)

data ShareBoard =
    ShareBoard { sb_boardId   :: !BoardId
               , sb_otherUser :: !UserId
               } deriving Generic

instance FromJSON ShareBoard where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }

routeShareBoard :: StorageApi
                -> ShareBoard
                -> UserId
                -> ExceptT ErrorResponse IO ()
routeShareBoard storageApi (ShareBoard boardId otherUser) userId = do

    isOneOfMyBoards

    otherUserExists

    catchAll "Could not share board." $

               -- Yep.  This should look less weird though  
        lift $ createUser storageApi otherUser boardId

    where
    isOneOfMyBoards :: ExceptT ErrorResponse IO () -- Could be done cheaper
    isOneOfMyBoards = do
        myBoardIds <- map fst <$> lift (getBoards storageApi userId)
        unless (boardId `elem` myBoardIds) $ err 403 "Not your board!"

    otherUserExists :: ExceptT ErrorResponse IO ()
    otherUserExists = pure () -- TODO
