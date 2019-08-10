module AppCtx where

import Database.Persist.Sql (ConnectionPool)
import Katip
import Network.HTTP.Client
import Servant.Auth.Server

import Settings

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data KatipState = KatipState
    { katipStateNamespace :: Namespace
    , katipStateContext :: LogContexts
    , katipStateLogEnv :: LogEnv
    }

data AppCtx = AppCtx
    { appCtxSettings       :: Settings
    , appCtxDatabasePool   :: ConnectionPool -- ^ Database connection pool.
    , appCtxHttpManager    :: Manager
    , appCtxKatipState     :: KatipState
    , appCtxCookieSettings :: CookieSettings
    , appCtxJWTSettings    :: JWTSettings
    }
