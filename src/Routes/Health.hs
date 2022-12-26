{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric  #-}

module Routes.Health (Health, HealthApi (..), create) where

import Types.Json                  (chop)

import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Time.Clock
import           Data.UUID         (UUID)
import qualified Data.UUID.V4 as U
import           GHC.Generics      (Generic)

newtype HealthApi = 
    HealthApi { getHealth :: IO Health }

create :: IO HealthApi
create = do

    upSince <- getCurrentTime

    instanceId <- InstanceId <$> U.nextRandom

    healthHits <- newTVarIO 0

    let getHealthImpl = do
            hits' <- atomically $ do hits <- readTVar healthHits
                                     let hits' = hits + 1
                                     writeTVar healthHits hits'
                                     pure hits'
            pure $ Health instanceId upSince hits'

    pure (HealthApi getHealthImpl)

newtype InstanceId =
    InstanceId UUID deriving (Generic, ToJSON)

data Health =
    Health { h_instanceId :: !InstanceId
           , h_upSince    :: !UTCTime 
           , h_healthHits :: !Int
           } deriving Generic

instance ToJSON Health where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True }
