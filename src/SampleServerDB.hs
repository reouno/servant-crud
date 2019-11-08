{-# LANGUAGE DataKinds                  #-}
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
{-# LANGUAGE TypeOperators              #-}

module SampleServerDB where

import           Control.Exception.Safe
import           Control.Monad.IO.Class  ( MonadIO, liftIO )
import           Control.Monad.Logger    ( runStderrLoggingT )
import           Data.Aeson              ( FromJSON, ToJSON )
import           Data.String.Conversions ( cs )
import           Data.Time               ( UTCTime )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics            ( Generic )
import           Servant

import           PersistentUtil
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

type UserAPI = "users" :> APIFor User Int

userAPI :: Proxy UserAPI
userAPI = Proxy

type UserId' = Int

type UserRecord = (UserId', User)

type Pool = ConnectionPool

mkPool :: FilePath -> IO Pool
mkPool filePath = mkSqlitePool filePath 10

initialize :: Pool -> IO ()
initialize = runSqlPool (runMigration migrateAll)

getUsers :: Pool -> IO [UserRecord]
getUsers pool = map entity2Tuple <$> selectList' pool

newUser :: Pool -> User -> IO UserId'
newUser pool user = sqlKey2Int <$> insert' user pool

-- FIXME: should implement appropriate error handling, not to raise runtime exception
getUser :: Pool -> UserId' -> IO User
getUser pool userId = do
  mayUser <- get' (int2SqlKey userId) pool
  case mayUser of
    Just user -> return user
    Nothing   -> error $ "user id `" ++ show userId ++ "` not found in DB table"

getUser' :: (MonadThrow m, MonadIO m) => Pool -> UserId' -> m User
getUser' pool userId = do
  mayUser <- liftIO $ runSqlPool (get (int2SqlKey userId)) pool
  case mayUser of
    Just user -> return user
    Nothing ->
      throw $
      KeyNotFoundException $
      "user id `" ++ show userId ++ "` not found in DB table"

updateUser :: Pool -> UserId' -> User -> IO ()
updateUser pool userId user = replace' (int2SqlKey userId) user pool

deleteUser :: Pool -> UserId' -> IO ()
deleteUser pool userId = delete' (int2SqlKey userId :: Key User) pool

userServer :: Pool -> Server UserAPI
userServer pool = getEntities pool :<|> newEntity pool :<|> operations pool
  where
    getEntities = liftIO . getUsers
    newEntity pool user = liftIO $ newUser pool user >> return NoContent
    operations pool id' =
      getEntity pool id' :<|> updateEntity pool id' :<|> deleteEntity pool id'
    getEntity pool id' = liftIO $ getUser pool id'
    updateEntity pool id' user =
      liftIO $ updateUser pool id' user >> return NoContent
    deleteEntity pool id' = liftIO $ deleteUser pool id' >> return NoContent

server :: IO (Server UserAPI)
server = do
  let _database_ = "SampleServerDB-dev"
  pool <- mkPool _database_
  initialize pool
  newUser pool user1
  return $ userServer pool

app :: IO Application
app = serve userAPI <$> server
{-
sample POST request:

curl -XPOST \
-H 'Content-Type: application/json' \
-H 'Accept: application/json' \
-d '{"userFirstName": "Isaac", "userLastName": "Newton", "userEmail": "isaac@newton.co.uk", "userRegistrationTimeStamp": "1683-03-01T00:00:00Z"}' \
127.0.0.1:8081/users

-}
