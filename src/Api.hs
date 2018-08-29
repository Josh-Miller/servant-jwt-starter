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

import           Config                      (AppT (..), Config)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Reader        (liftIO, runReaderT)
import           Crypto.BCrypt               (hashPasswordUsingPolicy,
                                              slowerBcryptHashingPolicy,
                                              validatePassword)
import           Data.Aeson
import           Data.Int                    (Int64)
import           Data.Text
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           GHC.Generics
import           Models                      (User (User), runDb, userEmail,
                                              userName)
import qualified Models                      as Md
import           Servant

data GenUser = GenUser
  { email    :: Text
  , password :: Text
  } deriving (Generic)

instance ToJSON GenUser

instance FromJSON GenUser

type UserApi
   = "users" :> Get '[ JSON] [Entity User] :<|> "user" :> Capture "name" Text :> Get '[ JSON] (Entity User)

type UnprotectedUserApi
   = "register" :> ReqBody '[ JSON] User :> Post '[ JSON] Int64 :<|> "login" :> ReqBody '[ JSON] GenUser :> Post '[ JSON] (Entity User)

appServer :: Config -> Server UserApi
appServer cfg = hoistServer userApi (convertApp cfg) userServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

userApi :: Proxy UserApi
userApi = Proxy

allUsers :: MonadIO m => AppT m [Entity User]
allUsers = runDb (selectList [] [])

singleUser :: MonadIO m => Text -> AppT m (Entity User)
singleUser name = do
  user <- runDb (selectFirst [Md.UserName ==. name] [])
  case user of
    Nothing -> throwError err404
    Just x  -> return x

registerUser :: MonadIO m => User -> AppT m Int64
registerUser u = do
  hashPass <-
    liftIO $
    hashPasswordUsingPolicy
      slowerBcryptHashingPolicy
      (encodeUtf8 (Md.userPassword u))
  case hashPass of
    Nothing -> throwError err404
    Just hashPass' ->
      fromSqlKey <$>
      runDb (insert (User (userName u) (userEmail u) (decodeUtf8 hashPass')))

loginUser :: MonadIO m => GenUser -> AppT m (Entity User)
loginUser u = do
  user <- runDb (selectFirst [Md.UserEmail ==. email u] [])
  case user of
    Nothing -> throwError err404
    Just (Entity userId x) ->
      if isValidPass
        then return $ Entity userId x
        else throwError $ err401 {errBody = "Your credentials are invalid."}
      where isValidPass =
              validatePassword
                (encodeUtf8 $ Md.userPassword x)
                (encodeUtf8 $ password u)

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserApi (AppT m)
userServer = allUsers :<|> singleUser

unprotectedUserServer :: MonadIO m => ServerT UnprotectedUserApi (AppT m)
unprotectedUserServer = registerUser :<|> loginUser

app :: Config -> Application
app cfg = serve userApi (appServer cfg)
