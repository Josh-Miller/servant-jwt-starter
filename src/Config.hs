{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Database.Persist.Postgresql
  ( ConnectionPool
  , ConnectionString
  , createPostgresqlPool
  )
import Servant (ServantErr)

data Config = Config
  { configPort :: Int
  , configPool :: ConnectionPool
  }

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Config
             , MonadError ServantErr
             , MonadIO
             )

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=test dbname=test password=test"

makePool :: IO ConnectionPool
makePool = runStdoutLoggingT $ createPostgresqlPool connStr 1
