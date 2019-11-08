{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module PersistentUtil
  ( mkSqlitePool
  , selectList'
  , get'
  , insert'
  , replace'
  , update'
  , delete'
  , sqlKey2Int
  , int2SqlKey
  , entity2Tuple
  , KeyNotFoundException(..)
  ) where

import           Basement.IntegralConv   ( int64ToInt, intToInt64 )
import           Conduit                 ( MonadUnliftIO )
import           Control.Exception.Safe
import           Control.Monad.Logger    ( runStderrLoggingT )
import           Data.Pool               ( Pool )
import           Data.String.Conversions ( cs )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH

type NumConnection = Int

mkSqlitePool :: FilePath -> NumConnection -> IO ConnectionPool
mkSqlitePool fp n = runStderrLoggingT $ createSqlitePool (cs fp) n

{-
 - CRUD functions
 -}
selectList' ::
     ( PersistQueryRead backend
     , IsPersistBackend backend
     , MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , BaseBackend backend ~ SqlBackend
     )
  => Data.Pool.Pool backend
  -> m [Entity record]
selectList' = runSqlPool $ selectList [] []

get' ::
     ( PersistStoreRead backend
     , IsPersistBackend backend
     , MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , BaseBackend backend ~ SqlBackend
     )
  => Key record
  -> Data.Pool.Pool backend
  -> m (Maybe record)
get' = runSqlPool . get

insert' ::
     ( PersistStoreWrite backend
     , IsPersistBackend backend
     , MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , BaseBackend backend ~ SqlBackend
     )
  => record
  -> Data.Pool.Pool backend
  -> m (Key record)
insert' = runSqlPool . insert

replace' ::
     ( PersistStoreWrite backend
     , IsPersistBackend backend
     , MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , BaseBackend backend ~ SqlBackend
     )
  => Key record
  -> record
  -> Data.Pool.Pool backend
  -> m ()
replace' k r = runSqlPool $ replace k r

update' ::
     ( PersistStoreWrite backend
     , IsPersistBackend backend
     , MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , BaseBackend backend ~ SqlBackend
     )
  => Key record
  -> [Update record]
  -> Data.Pool.Pool backend
  -> m record
update' k us = runSqlPool $ updateGet k us

delete' ::
     ( PersistStoreWrite backend
     , IsPersistBackend backend
     , MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , BaseBackend backend ~ SqlBackend
     )
  => Key record
  -> Data.Pool.Pool backend
  -> m ()
delete' = runSqlPool . delete

{-
 - data conversion functions
 -}
sqlKey2Int :: ToBackendKey SqlBackend record => Key record -> Int
sqlKey2Int = int64ToInt . fromSqlKey

int2SqlKey :: ToBackendKey SqlBackend record => Int -> Key record
int2SqlKey = toSqlKey . intToInt64

entity2Tuple :: ToBackendKey SqlBackend b => Entity b -> (Int, b)
entity2Tuple entity = (sqlKey2Int . entityKey $ entity, entityVal entity)

{-
 - Exceptions
 -}
newtype KeyNotFoundException =
  KeyNotFoundException String
  deriving (Show)

instance Exception KeyNotFoundException
