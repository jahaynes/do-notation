module Types.Json (chop) where

import Data.List.Split            (splitOn)

chop :: String -> String
chop = concat . drop 1 . splitOn "_"