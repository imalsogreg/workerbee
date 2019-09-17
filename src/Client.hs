module Client where

import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

import API
import TaskTree

(hc :<|> state :<|> hire :<|> winLotto :<|> asgn) = client api

defaultEnv :: IO ClientEnv
defaultEnv = do
  mgr <- newManager defaultManagerSettings
  return $ mkClientEnv mgr (BaseUrl Http "localhost" 8080 [])
