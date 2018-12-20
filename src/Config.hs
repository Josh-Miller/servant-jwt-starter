{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config where

import           Control.Monad.Except        (ExceptT, MonadError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              createPostgresqlPool)
import           Katip                       (LogEnv, runKatipT)
import           Servant                     (ServantErr)
import           Servant.Auth.Server         (CookieSettings, JWTSettings)

data Config = Config
  { configPort   :: Int
  , configPool   :: ConnectionPool
  , configJwt    :: JWTSettings
  , configCookie :: CookieSettings
  , configEnv    :: Environment
  , configLogEnv :: LogEnv
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

data Environment
  = Local
  | Development
  | Staging
  | Production
  deriving (Eq, Show, Ord, Read)

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=test dbname=test password=test"

makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Local env =
  runKatipT env $ runStdoutLoggingT $ createPostgresqlPool connStr 1
makePool Development env =
  runKatipT env $ runStdoutLoggingT $ createPostgresqlPool connStr 1
makePool Staging env =
  runKatipT env $ runStdoutLoggingT $ createPostgresqlPool connStr 1
makePool Production env =
  runKatipT env $ runStdoutLoggingT $ createPostgresqlPool connStr 1
