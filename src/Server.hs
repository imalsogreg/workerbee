{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import qualified Algebra.Graph            as G
import qualified Control.Concurrent.STM   as STM
import qualified Control.Exception        as Exception
import qualified Control.Monad.Except     as Except
import qualified Control.Monad.IO.Class   as IO
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as List
import qualified Data.Set                 as Set
import qualified Control.Monad.Writer     as Writer
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Dhall
import           GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant                  as Servant
import           Servant.API
import qualified Servant.Server           as Servant
import qualified System.Environment       as Env

import           API
import qualified Config
import           Docs
import           TaskTree
import           RenderGraph

data ServerConfig = ServerConfig
  { taskTree :: Config.Config
  , hostname :: Text.Text
  , port     :: Integer
  , autosave :: FilePath
  } deriving (Show, Generic)

instance Dhall.Interpret ServerConfig

apiServer :: STM.TVar State -> Servant.Server API
apiServer stateVar = healthcheck :<|> getState :<|> hireE :<|> lotto :<|> assn
  where
    healthcheck  = return undefined
    getState     = IO.liftIO $ STM.readTVarIO stateVar
    hireE worker = IO.liftIO $ do
      STM.atomically (STM.modifyTVar stateVar (hire worker))
      return NoContent

    lotto worker  = do
      res <- IO.liftIO $ STM.atomically $ do
        s <- STM.readTVar stateVar
        if Set.member worker (workers s)
          then (do
                   STM.writeTVar stateVar (fst (winLottery worker s))
                   return (Right ())
               )
          else return (Left $ "No worker named " <> unWorker worker)

      case res of
        Left e ->
          Except.throwError Servant.err404 { Servant.errBody = textToLBS e }
        Right _ ->
          return NoContent

    assn (Just worker) (Just task) = do
      res <- IO.liftIO $ STM.atomically $ do
        s <- STM.readTVar stateVar
        let (s', displaced) = assign (Assignment task worker) s

        let errors = Writer.execWriter $ do
              let tattle pred msg = Writer.when pred (Writer.tell [msg])
              tattle (Set.notMember worker (workers s))
                ("No worker " <> unWorker worker <> " exists.")
              tattle (not (G.hasVertex task (tasks s)))
                ("No task " <> unTask task <> " exists.")
              tattle (not (Set.null displaced))
                ("Assigning " <> unTask task <> " to "
                  <> unWorker worker
                  <> " conflicts with existing assignments: "
                  <> Text.pack (show displaced))

        if List.null errors
          then STM.writeTVar stateVar s' >> return (Right NoContent)
          else return (Left (Text.unlines errors))

      case res of
        Left e ->
          Except.throwError Servant.err404 { Servant.errBody = textToLBS e }
        Right a ->
          return a


siteServer :: STM.TVar State -> Servant.Server Site
siteServer stateVar = apiServer stateVar :<|> statePage :<|> docsPage
  where
    statePage = IO.liftIO $ do
      s <- STM.readTVarIO stateVar
      imageForState s
    docsPage = IO.liftIO apiDocs


textToLBS :: Text.Text -> LBS.ByteString
textToLBS = LBS.fromStrict . Text.encodeUtf8

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [fp] -> mainWithConfig (Text.pack fp)
    _  ->
      putStrLn $ unwords [ "Usage: workerbee CONFIG_FILE" ]


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
     (Servant.serve site (siteServer state)))

    (LBS.writeFile (autosave cfg) . Aeson.encode =<< STM.readTVarIO state)
