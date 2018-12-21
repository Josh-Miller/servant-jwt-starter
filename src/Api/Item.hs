{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Item where

import           Config                      (AppT (..), Config, configCookie,
                                              configJwt)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Reader        (ask, liftIO, runReaderT)
import           Crypto.BCrypt               (hashPasswordUsingPolicy,
                                              slowerBcryptHashingPolicy,
                                              validatePassword)
import           Data.Aeson
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import           Data.Text
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           GHC.Generics
import           Models                      (Item (Item), User (User), runDb)
import qualified Models                      as Md
import           Servant
import           Servant.Auth.Server

type ProtectedItemApi
   = "item" :> ReqBody '[ JSON] Item :> Post '[ JSON] (Md.Key Item)

type UnprotectedItemApi
   = "item" :> Capture "id" Text :> Get '[ JSON] (Entity Item) :<|> "items" :> Get '[ JSON] [Entity Item]

getItem :: MonadIO m => Text -> AppT m (Entity Item)
getItem id = do
  item <- runDb (selectFirst [Md.ItemName ==. id] [])
  case item of
    Nothing -> throwError err404
    Just x  -> return x

getItems :: MonadIO m => AppT m [Entity Item]
getItems = runDb (selectList [] [])

unprotectedItem :: MonadIO m => ServerT UnprotectedItemApi (AppT m)
unprotectedItem = getItem :<|> getItems

createItem :: MonadIO m => Item -> AppT m (Md.Key Item)
createItem x =
  runDb
    (insert
       (Item
          (Md.itemName x)
          (Md.itemDescription x)
          (Md.itemVariations x)
          (Md.itemModifiers x)))

protectedItem ::
     MonadIO m => AuthResult User -> ServerT ProtectedItemApi (AppT m)
protectedItem (Authenticated user) = createItem
protectedItem _                    = throwAll err401
