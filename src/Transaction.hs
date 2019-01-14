{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transaction where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (FromJSON, ToJSON, encode)
import           GHC.Generics              (Generic)
import           Network.HTTP.Client       (RequestBody (RequestBodyLBS),
                                            httpLbs, method, newManager,
                                            parseRequest, requestBody,
                                            responseStatus)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Method (methodPost)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO                 (IO (..))

type Limit = Int

type Money = Double

type Id = String

data Charge = Charge
  { amount      :: Money
  , currency    :: String
  , source      :: String
  , description :: String
  } deriving (Eq, Ord, Generic)

instance ToJSON Charge

instance FromJSON Charge

newtype CreditCardNumber a = UnsafeCreditCardNumber
  { unCreditCardNumber :: a
  }

countDigits :: Integral a => a -> [a]
countDigits 0 = []
countDigits x = countDigits (x `div` 10) ++ [x `mod` 10]

creditCardNumber :: (Num a, Eq a, Integral a) => a -> Maybe (CreditCardNumber a)
creditCardNumber n
  | length (countDigits n) < 16 = Nothing
  | length (countDigits n) == 16 = Just (UnsafeCreditCardNumber n)
  | length (countDigits n) > 16 = Nothing
  | otherwise = Nothing

data CreditCard a = CreditCard
  { number   :: CreditCardNumber a
  , expMonth :: Int
  , expYear  :: Int
  , cvc      :: String
  }

{- instance ToJSON CreditCard a-}
{- instance FromJSON CreditCard a-}
data ChargeRes = ChargeRes
  { id             :: Id
  , object         :: String
  , amountRefunded :: Money
  , application    :: Maybe String
  , paid           :: Bool
  } deriving (Eq, Ord, Generic)

instance ToJSON ChargeRes

instance FromJSON ChargeRes

class Monad m =>
      MonadTransaction m
  where
  charge :: Charge -> m ()
  getCharge :: Id -> m ChargeRes
  updateCharge :: Id -> Charge -> m ChargeRes
  captureCharge :: Id -> m ChargeRes
  listCharges :: Limit -> m [ChargeRes]

testProgram :: MonadTransaction m => m ChargeRes
testProgram = do
  charge $ Charge 2.5 "usd" "tok_534j" "Bought a sandwhich"
  test <- getCharge "sku_ji494"
  return test

instance MonadTransaction IO where
  charge = chargeCard
  getCharge x = undefined
  updateCharge x = undefined
  captureCharge x = undefined
  listCharges = undefined

chargeCard :: (MonadIO m, MonadThrow m) => Charge -> m ()
chargeCard x = do
  manager <- liftIO $ newManager tlsManagerSettings
  initReq <- parseRequest "https://api.stripe.com/v1/charges"
  let request =
        initReq {method = methodPost, requestBody = RequestBodyLBS $ encode x}
  response <- liftIO $ httpLbs request manager
  liftIO $
    putStrLn $
    "The status code was: " ++ (show $ statusCode $ responseStatus response)
  return ()
