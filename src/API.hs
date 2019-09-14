{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module API where

import qualified Data.Aeson as Aeson
import GHC.Generics
import           Data.Text   (Text)
import qualified Data.Text
import Data.Proxy
import           Servant.API

data HealthCheck = Ok
  deriving (Eq, Generic)

instance Aeson.ToJSON HealthCheck
instance Aeson.FromJSON HealthCheck

type API =
    "healthcheck" :> Get '[JSON] HealthCheck

api :: Proxy API
api = Proxy
