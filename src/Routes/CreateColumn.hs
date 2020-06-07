{-# LANGUAGE DeriveGeneric
           , LambdaCase
           , OverloadedStrings #-}

module Routes.CreateColumn where

import Errors
import Storage.StorageApi
import Types.Board
import Types.BoardId
import Types.Column
import Types.Json
import Types.User

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Generics               (Generic)

data CreateColumn =
    CreateColumn { cc_boardId    :: !BoardId
                 , cc_columnName :: !ColumnName
                 } deriving Generic

instance FromJSON CreateColumn where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True }
 
routeCreateColumn :: StorageApi
                  -> CreateColumn
                  -> UserId
                  -> ExceptT ErrorResponse IO ColumnId
routeCreateColumn storageApi (CreateColumn boardId columnName) userId = do

    validate columnName

    -- TODO columnNotAlreadyExists

    catchAll "Could not create column." $ do
        
        lift (getBoard storageApi boardId) >>= \case
            
            Nothing -> err 400 "Cannot create board for non-existent board"
            
            Just board -> do let numColumns = V.length . b_columns $ board
                                 columnPosition = ColumnPosition (1 + numColumns)
                             lift (createColumn storageApi boardId columnPosition columnName)

    where
    validate :: ColumnName -> ExceptT ErrorResponse IO ()
    validate (ColumnName cn)
        | T.null cn        = err 400 "Column name was empty"
        | T.length cn > 40 = err 400 "Column name too long (40+)"
        | otherwise        = pure ()
