module Api.Main where

import RIO hiding (Handler(..))

import Control.Monad.Trans.Except
import Servant

import AppCtx

type Api = Get '[PlainText] NoContent

apiSpec :: Proxy Api
apiSpec = Proxy

type AppM = RIO AppCtx

apiImpl :: AppM NoContent
apiImpl = pure NoContent

interpretServant :: AppCtx -> AppM a -> Handler a
interpretServant appctx = Handler . ExceptT . try . runRIO appctx
