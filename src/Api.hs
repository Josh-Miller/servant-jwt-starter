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

import           Api.User             (GenUser (..), UnprotectedUserApi,
                                       UserApi, unprotectedUser)
import           Config               (AppT (..), Config, configCookie,
                                       configJwt)
import           Control.Monad.Except (MonadIO)
import           Control.Monad.Reader (runReaderT)
import           Models               (User (User))
import           Servant
import           Servant.Auth.Server  (Auth, Cookie, CookieSettings, JWT,
                                       JWTSettings)

type Api = UnprotectedUserApi

api :: Proxy Api
api = Proxy

server :: MonadIO m => Config -> ServerT Api (AppT m)
server cfg = unprotectedUser

allServer :: Config -> Server Api
allServer cfg = hoistServer api (convertApp cfg) (server cfg)

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

app :: Config -> Application
app cfg = serve api (allServer cfg)
