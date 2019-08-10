{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( appMain
    , getAppSettings
    , makeFoundation
    , makeLogWare
    )
where

import RIO hiding (LogLevel(..), logInfoS)

import Crypto.JOSE.JWK
import Data.Yaml.Config
import Database.Persist.Postgresql
import Katip hiding (Environment(..))
import qualified Katip (Environment(..))
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
    (Settings, defaultSettings, defaultShouldDisplayException, setHost, setOnException, setPort)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger
    (IPAddrSource(..), OutputFormat(..), Destination(..), destination, mkRequestLogger, outputFormat)
import Servant
import Servant.Auth.Server
import qualified System.IO as IO
import qualified System.Log.FastLogger as FastLogger

import Api.Main
import AppCtx
import ConfigPaths
import Model
import Orphans.Katip ()
import Settings

defaultMiddlewaresNoLogging :: Middleware
defaultMiddlewaresNoLogging = acceptOverride . autohead . gzip def . methodOverride

defaultLogEnv :: Environment -> IO LogEnv
defaultLogEnv environment = do
    handleScribe <- mkHandleScribe ColorIfTerminal IO.stdout (permitItem DebugS) V2
    env <- initLogEnv "JakkCapital" (Katip.Environment . tshow $ environment)
    registerScribe "stdout" handleScribe defaultScribeSettings env

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: Settings -> IO AppCtx
makeFoundation appCtxSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    jwk <- genJWK (ECGenParam P_256)
    let appCtxCookieSettings = defaultCookieSettings
    let appCtxJWTSettings = defaultJWTSettings jwk
    appCtxHttpManager <- getGlobalManager
    appCtxKatipState <- KatipState mempty mempty <$> defaultLogEnv (settingsEnvironment appCtxSettings)

    -- Create the database connection pool
    appCtxDatabasePool <-
        runKatipContextT (katipStateLogEnv appCtxKatipState) () "Database"
            $ (liftA2 createPostgresqlPool pgConnStr pgPoolSize $ settingsDatabaseConf appCtxSettings)

    runKatipT (katipStateLogEnv appCtxKatipState) $ runSqlPool (runMigration migrateAll) appCtxDatabasePool

    pure AppCtx { .. }

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: AppCtx -> IO Application
makeApplication appctx = do
    logWare <- makeLogWare appctx
    -- Create the WAI application and apply middlewares
    jwk <- genJWK (ECGenParam P_256)
    let cfg = defaultCookieSettings :. defaultJWTSettings jwk :. EmptyContext
    let
        appPlain = serveWithContext apiSpec cfg
            $ hoistServerWithContext
                  apiSpec
                  (Proxy :: Proxy '[CookieSettings, JWTSettings])
                  (interpretServant appctx)
                  apiImpl
    pure . logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: AppCtx -> IO Middleware
makeLogWare appCtx = mkRequestLogger def
    { outputFormat = if settingsDetailedRequestLogging $ appCtxSettings appCtx then Detailed True else Apache FromSocket
    , destination =
        Callback
        $ runKatipT (katipStateLogEnv $ appCtxKatipState appCtx)
        . logMsg "Warp" InfoS
        . logStr
        . FastLogger.fromLogStr
    }

-- | Warp settings for the given foundation value.
warpSettings :: AppCtx -> Warp.Settings
warpSettings ctx =
    Warp.setPort (settingsPort $ appCtxSettings ctx)
        $ Warp.setHost (settingsHost $ appCtxSettings ctx)
        $ Warp.setOnException
              (\_req e ->
                  when (Warp.defaultShouldDisplayException e)
                      $ runKatipContextT (katipStateLogEnv $ appCtxKatipState ctx) () "servant"
                      $ $logTM ErrorS ("Exception from Warp: " <> showLS e)
              )
              Warp.defaultSettings

getAppSettings :: IO Settings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runTLS (tlsSettings "config/certificate.pem" "config/key.pem") (warpSettings foundation) app
