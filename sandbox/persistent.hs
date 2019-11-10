:set -XGADTs
:set -XScopedTypeVariables

import           Basement.IntegralConv
import           Database.Persist.Sql

-- make DB connection and do migration
pool <- mkPool "sandbox-persistent"
initialize pool

-- insert one record at first
insert' user1 pool

-- get all records
-- have to specify the type of the return data
-- need GADTs extension
userEnts <- selectList' pool :: IO [Entity User]

-- need no type declaration, actually
users = map entityVal userEnts :: [User] 

userRecords <- map entity2Tuple <$> selectList' pool :: IO [(Int, User)]


{-
 - sample code using safe-exceptions
 -}
import           Control.Exception.Safe
