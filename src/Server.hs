{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified System.Environment as Env
import qualified Data.Text as Text
import GHC.Generics
import Servant.API
import qualified Servant.Server as Servant
import qualified Dhall
import qualified Network.Wai.Handler.Warp as Warp

import API
import qualified Config
import TaskTree

data ServerConfig = ServerConfig
  { taskTree :: Config.Config
  , hostname :: Text.Text
  , port     :: Integer
  , autosave :: FilePath
  } deriving (Show, Generic)

instance Dhall.Interpret ServerConfig

server :: STM.TVar State -> Servant.Server API
server = undefined

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [exe, fp] -> mainWithConfig (Text.pack fp)
    (exe: _)  ->
      putStrLn $ unwords [ "Usage:", exe, "<CONFIG_FILE>" ]


mainWithConfig :: Text.Text -> IO ()
mainWithConfig cfgFile = do
  cfg <- Dhall.input Dhall.auto cfgFile

  let tryReadState :: FilePath -> IO State
      tryReadState fp = do
        let defaultCfg = Config.initialState (taskTree cfg)
        Exception.try (LBS.readFile fp) >>= \case
          Left (e :: Exception.SomeException) -> return defaultCfg
          Right bs -> case Aeson.decode bs of
            Nothing    -> return defaultCfg
            Just state -> return state

  state <- STM.newTVarIO =<< tryReadState (autosave cfg)
  Exception.finally
    (Warp.run (fromIntegral (port cfg))
     (Servant.serve api (server state)))

    (LBS.writeFile (autosave cfg) . Aeson.encode =<< STM.readTVarIO state)

  where
        
