{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module SampleServerDB
  ( server
  ) where

import           Control.Monad.Logger    ( runStderrLoggingT )
import           Data.Aeson              ( FromJSON, ToJSON )
import           Data.String.Conversions ( cs )
import           Data.Time               ( UTCTime )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics            ( Generic )
import           ServantCrud             ( APIFor )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  firstName String
  lastName String
  email String
  registrationTimeStamp UTCTime default=CURRENT_TIMESTAMP
  deriving Read Eq Generic Show
|]

instance FromJSON User

instance ToJSON User

user1 =
  User
    "Isaac"
    "Newton"
    "isaac@newton.co.uk"
    (read "1683-03-01 00:00:00" :: UTCTime)

user2 =
  User
    "Albert"
    "Einstein"
    "albert@mc2.org"
    (read "1905-12-01 00:00:01" :: UTCTime)

mkPool :: FilePath -> IO ConnectionPool
mkPool filePath = runStderrLoggingT $ createSqlitePool (cs filePath) 10

initialize :: ConnectionPool -> IO ()
initialize = runSqlPool (runMigration migrateAll)

server :: IO ()
server = do
  let _database_ = "SampleServerDB-dev"
  pool <- mkPool _database_
  initialize pool
