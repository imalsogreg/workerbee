{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module API where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics
import           Data.Text   (Text)
import Network.HTTP.Media ((//), (/:))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Proxy
import           Servant.API
import qualified Servant.Docs as Servant

import TaskTree

data HealthCheck = Ok
  deriving (Eq, Generic)

instance Aeson.ToJSON HealthCheck
instance Aeson.FromJSON HealthCheck

instance Servant.ToSample HealthCheck where
  toSamples _ = Servant.singleSample Ok

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

data HTMLPage = HTMLPage Text

-- instance Servant.ToSample HTMLPage where
--   toSamples _ = Servant.singleSample (HTMLPage "<Some HTML page>")

instance MimeRender HTML HTMLPage where
  mimeRender _ (HTMLPage val) = LBS.fromStrict $ Text.encodeUtf8 val


type API =
      "healthcheck" :> Get '[JSON] HealthCheck
 :<|> "state"       :> Get '[JSON] State
 :<|> "hire"        :> Capture "worker" Worker :> Post   '[JSON] NoContent
 :<|> "win-lottery" :> Capture "worker" Worker :> Delete '[JSON] NoContent
 :<|> "assign"      :> QueryParam "worker" Worker
                    :> QueryParam "task"   Task
                    :> Post       '[JSON] NoContent


api :: Proxy API
api = Proxy

type Site =
  API
  :<|> "state.html"  :> Get '[HTML] HTMLPage
  :<|> "docs.html"   :> Get '[HTML] HTMLPage

site :: Proxy Site
site = Proxy
