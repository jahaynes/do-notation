module Storage.Cassandra.Common where

import Database.CQL.IO

params :: a -> QueryParams a
params p = QueryParams
    { consistency       = One
    , skipMetaData      = False
    , values            = p
    , pageSize          = Nothing
    , queryPagingState  = Nothing
    , serialConsistency = Nothing
    , enableTracing     = Nothing
    }
