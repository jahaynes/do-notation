module Storage.Cassandra.Connection (create) where

import Prelude hiding (init)

import Database.CQL.IO
import Network.Socket  (PortNumber)

create :: PortNumber -> [String] -> IO ClientState
create    _       [] = error "FATAL.  No Cassandra hosts supplied"
create port (h:osts) = init
                     . setPortNumber port 
                     . setContacts h osts
                     $ defSettings
