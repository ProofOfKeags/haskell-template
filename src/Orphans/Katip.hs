{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans.Katip where

import RIO hiding (LogLevel(..), LogSource)

import Control.Monad.Logger as Logger
import Katip
import System.Log.FastLogger as FastLogger

instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = adapt logMsg

instance MonadIO m => MonadLogger (KatipContextT m) where
    monadLoggerLog = adapt logMsg

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug = DebugS
fromLevel LevelInfo = InfoS
fromLevel LevelWarn = WarningS
fromLevel LevelError = ErrorS
fromLevel (LevelOther _) = NoticeS

-- | Transforms Katip logMsg into monadLoggerLog to be used inside
-- MonadLogger monad
adapt
    :: (ToLogStr msg, Applicative m, Katip m)
    => (Namespace -> Severity -> Katip.LogStr -> m ())
    -> Loc
    -> LogSource
    -> LogLevel
    -> msg
    -> m ()
adapt f _ src lvl msg = f ns (fromLevel lvl) $ logStr' msg
    where
        ns = Namespace [src]
        -- not sure how fast this is going to be
        logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
