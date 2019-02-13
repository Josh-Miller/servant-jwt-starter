{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Api.Item             (ProtectedItemApi, UnprotectedItemApi,
                                       protectedItem, unprotectedItem)
import           Api.Organization     (OrganizationApi, protectedOrganization)
import           Api.User             (GenUser (..), UnprotectedUserApi,
                                       UserApi, protectedUser, unprotectedUser)
import           Config               (AppT (..), Config, configCookie,
                                       configJwt)
import           Control.Monad.Except (MonadIO)
import           Control.Monad.Reader (runReaderT)
import           Models               (User (User))
import           Servant
import           Servant.Auth.Server  (Auth, Cookie, CookieSettings, JWT,
                                       JWTSettings)

type Api auths
   = (Auth auths User :> UserApi) :<|> (Auth auths User :> OrganizationApi) :<|> UnprotectedUserApi

api :: Proxy (Api '[ JWT, Cookie])
api = Proxy

server :: MonadIO m => Config -> ServerT (Api auths) (AppT m)
server cfg =
  protectedUser :<|> protectedOrganization :<|>
  unprotectedUser (configCookie cfg) (configJwt cfg)

allServer :: Config -> Server (Api auths)
allServer cfg =
  hoistServerWithContext
    api
    (Proxy :: Proxy '[ CookieSettings, JWTSettings])
    (convertApp cfg)
    (server cfg)

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

app :: Config -> Application
app cfg =
  serveWithContext
    api
    (configCookie cfg :. configJwt cfg :. EmptyContext)
    (allServer cfg)
