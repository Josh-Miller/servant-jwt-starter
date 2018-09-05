{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Organization where

import           Config                      (AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Models                      (Organization (Organization),
                                              User (User), runDb)
import qualified Models                      as Md
import           Servant
import           Servant.Auth.Server

type OrganizationApi
   = "organizations" :> Get '[ JSON] [Entity Organization] :<|> "organization" :> Capture "name" Text :> Get '[ JSON] (Entity Organization)

allOrganizations :: MonadIO m => AppT m [Entity Organization]
allOrganizations = runDb (selectList [] [])

singleOrganization :: MonadIO m => Text -> AppT m (Entity Organization)
singleOrganization name = do
  org <- runDb (selectFirst [Md.OrganizationName ==. name] [])
  case org of
    Nothing -> throwError err404
    Just x  -> return x

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protectedOrganization ::
     MonadIO m => AuthResult User -> ServerT OrganizationApi (AppT m)
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protectedOrganization (Authenticated user) =
  allOrganizations :<|> singleOrganization
-- Otherwise, we return a 401.
protectedOrganization _ = throwAll err401
