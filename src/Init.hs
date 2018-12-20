module Init where

import           Api                         (app)
import           Config
import           Control.Exception           (bracket)
import           Data.Pool                   (destroyAllResources)
import           Database.Persist.Postgresql (runSqlPool)
import           Katip                       (closeScribes)
import           Logger                      (defaultLogEnv)
import           Models
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Safe
import           Servant.Auth.Server         (defaultCookieSettings,
                                              defaultJWTSettings, generateKey)
import           System.Environment          (lookupEnv)

runApp :: IO ()
runApp = bracket getConfig shutdown runApp
  where
    runApp config = run (configPort config) =<< initialize config

shutdown cfg = do
  closeScribes (configLogEnv cfg)
  destroyAllResources (configPool cfg)
  pure ()

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg = do
  runSqlPool doMigrations (configPool cfg)
  pure $ app cfg

getConfig :: IO Config
getConfig = do
  port <- lookupSetting "PORT" 4002
  env <- lookupSetting "ENVIRONMENT" Local
  logEnv <- defaultLogEnv
  pool <- makePool env logEnv
  key <- generateKey
  return
    Config
      { configPort = port
      , configPool = pool
      , configJwt = defaultJWTSettings key
      , configCookie = defaultCookieSettings
      , configEnv = env
      , configLogEnv = logEnv
      }

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeVal <- lookupEnv env
  case maybeVal of
    Nothing -> return def
    Just x  -> maybe (handleFail x) return (readMay x)
  where
    handleFail x =
      error $ mconcat ["Failed to read ", x, " for environment ", env]
