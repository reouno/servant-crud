{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module SampleServer
  ( app
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class ( liftIO )
import           Data.Aeson
import           Data.Time              ( UTCTime )
import           GHC.Generics           ( Generic )
import           Servant

import           ServantCrud            ( APIFor )

data User =
  User
    { userFirstName             :: String
    , userLastName              :: String
    , userEmail                 :: String
    , userRegistrationTimeStamp :: UTCTime
    }
  deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

userId1 = 1 :: Int

user1 =
  User
    "Isaac"
    "Newton"
    "isaac@newton.co.uk"
    (read "1683-03-01 00:00:00" :: UTCTime)

userId2 = 2 :: Int

user2 =
  User
    "Albert"
    "Einstein"
    "albert@mc2.org"
    (read "1905-12-01 00:00:00" :: UTCTime)

users = [(userId1, user1), (userId2, user2)]

type UserAPI = "users" :> APIFor User Int

userAPI :: Proxy UserAPI
userAPI = Proxy

type UserId = Int

type UserRecord = (UserId, User)

type Pool = TVar [UserRecord]

initUserPool :: IO Pool
initUserPool = atomically $ newTVar users

getUsers :: Pool -> IO [UserRecord]
getUsers = readTVarIO

newUser :: Pool -> User -> IO UserId
newUser pool user = do
  users <- getUsers pool
  let newId = (maximum . map fst) users + 1
      updatedUsers = users ++ [(newId, user)]
  atomically $ writeTVar pool updatedUsers
  return newId

-- FIXME: should not throw runtime exception even if user ID not found
getUser :: Pool -> UserId -> IO User
getUser pool id' = do
  users <- getUsers pool
  return $ [user | (userId, user) <- users, userId == id'] !! 0

-- FIXME: should not throw runtime exception even if user ID not found
updateUser :: Pool -> UserId -> User -> IO ()
updateUser pool id' newUser = do
  users <- getUsers pool
    -- just check the existence of id'
  let _ = [user | (userId, user) <- users, userId == id'] !! 0
      replaceUser (userId, user) userId' newUser =
        case userId of
          userId' -> (userId, newUser)
          _       -> (userId, user)
      updatedUsers = map (\(i, u) -> replaceUser (i, u) id' newUser) users
  atomically $ writeTVar pool updatedUsers

deleteUser :: Pool -> UserId -> IO ()
deleteUser pool id' = do
  users <- getUsers pool
  let updatedUsers = [x | x <- users, fst x /= id']
  atomically $ writeTVar pool updatedUsers

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
server = userServer <$> initUserPool

app :: IO Application
app = serve userAPI <$> server
