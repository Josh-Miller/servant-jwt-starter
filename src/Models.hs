{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Config               (Config, configPool)
import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import           Data.Text            (Text)
import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           GHC.Generics
import           Servant.Auth.Server

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User json
    name Text
    email Text
    password Text
    organizationId OrganizationId Maybe
    deriving Show Eq Generic
  Organization json
    name Text
    accountOwner UserId
    logo Text Maybe
    deriving Show Eq Generic
  Variation json
    name Text
    price Double
    images [ImageUrl]
    sku Text Maybe
    deriving Show Eq Generic
  Modifier json
    name Text
    price Double
    images [ImageUrl]
    deriving Show Eq Generic
  Item json
    name Text
    description Text
    variations [Variation]
    modifiers [Modifier]
    deriving Show Eq Generic
|]

instance ToJWT User

instance FromJWT User

type ImageUrl = Text

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
