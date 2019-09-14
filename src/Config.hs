{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import qualified Data.Aeson as Aeson
import qualified Algebra.Graph as G
import qualified Data.Set      as Set
import qualified Dhall
import           GHC.Generics

import qualified TaskTree

data Config = Config
  { dependencies :: [(TaskTree.Task, TaskTree.Task)]
  , workers      :: [TaskTree.Worker]
  } deriving (Show, Eq, Generic)

instance Dhall.Interpret Config where

instance Aeson.ToJSON Config
instance Aeson.FromJSON Config

initialState :: Config -> TaskTree.State
initialState Config{..} = TaskTree.State
  { TaskTree.workers     = Set.fromList workers
  , TaskTree.tasks       = G.edges dependencies
  , TaskTree.assignments = mempty
  }
