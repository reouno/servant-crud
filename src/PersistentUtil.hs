{-# LANGUAGE GADTs #-}

module PersistentUtil
  (
  ) where

import           Conduit                 ( MonadUnliftIO )
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
update' k rs = runSqlPool $ updateGet k rs

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
