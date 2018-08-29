module Init where

import           Api                         (app)
import           Config
import           Control.Exception           (bracket)
import           Database.Persist.Postgresql (runSqlPool)
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

shutdown config = pure ()

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg = do
  runSqlPool doMigrations (configPool cfg)
  pure $ app cfg

getConfig :: IO Config
getConfig = do
  port <- lookupSetting "PORT" 3000
  pool <- makePool
  key <- generateKey
  return
    Config
      { configPort = port
      , configPool = pool
      , configJwt = defaultJWTSettings key
      , configCookie = defaultCookieSettings
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
