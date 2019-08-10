{-# LANGUAGE CPP               #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import RIO
import RIO.Text (unpack)

import           ConfigPaths
import qualified Control.Exception           as Exception
import           Data.Aeson                  
import           Data.FileEmbed              (embedFile)
import           Data.Yaml                   (decodeEither')
import           Data.Yaml.Config            (applyEnvValue)
import           Database.Persist.Postgresql (PostgresConf)
import           Network.Wai.Handler.Warp    (HostPreference)

data Environment =
      Development
    | Production
    deriving (Eq, Ord, Show, Read)
instance FromJSON Environment where
    parseJSON = withText "Environment" $ \t ->
        maybe (fail $ "Invalid Environment" <> unpack t) pure . readMaybe $ unpack t

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data Settings = Settings
    { settingsDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , settingsHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , settingsPort                   :: Int
    -- ^ Port to listen on

    , settingsEnvironment            :: Environment
    -- ^ Dev/Prod
    , settingsDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , settingsShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    }

instance FromJSON Settings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        settingsDatabaseConf           <- o .: "database"
        settingsHost                   <- o .: "host" >>= pure . fromString
        settingsPort                   <- o .: "port"
        dev                            <- o .:? "development"      .!= defaultDev
        settingsEnvironment            <- o .: "environment"
        settingsDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        settingsShouldLogAll           <- o .:? "should-log-all"   .!= dev

        return Settings {..}

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: Settings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings
