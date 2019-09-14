{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module TaskTree where

import qualified Data.Aeson as Aeson
import qualified Algebra.Graph as G
import           Data.List     (partition)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Text     (Text)
import qualified Dhall         as Dhall
import           GHC.Generics  (Generic)

newtype Task = Task {
  unTask :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Task
instance Aeson.FromJSON Task

newtype Worker = Worker {
  unWorker :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Worker
instance Aeson.FromJSON Worker

data Assignment = Assignment
  { task   :: Task
  , worker :: Worker
  } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Assignment
instance Aeson.FromJSON Assignment

data State = State
  { tasks       :: G.Graph Task
  , workers     :: Set Worker
  , assignments :: Set Assignment
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON State where
  parseJSON = Aeson.withObject "State" $ \o ->
    State
    <$> fmap G.edges (o Aeson..: "tasks")
    <*> o Aeson..: "workers"
    <*> o Aeson..: "assignments"

instance Aeson.ToJSON State where
  toJSON State{..} = Aeson.object
    [ "tasks"      Aeson..= G.edgeList tasks
    , "workers"    Aeson..= workers
    , "assignmens" Aeson..= assignments
    ]

-- | Assign a user to a task, displacing old assignments
--   involving the same worker or task
assign :: Assignment -> State -> (State, Set Assignment)
assign a State{..} =
  (State {assignments = assignments', ..}, droppedAssignments)
  where
    -- Add the new assignment, and remove old asignments
    -- that shared either the worker or the task
    (droppedAssignments, remainingAssignments) =
      Set.partition (\a' -> task a'   == task   a
                         || worker a' == worker a
                    ) assignments

    assignments' = Set.insert a remainingAssignments

-- | Add a worker to the worker pool
hire :: Worker -> State -> State
hire w State{..} = State
  { workers = Set.insert w workers, .. }

-- | Remove a worker from the worker pool
winLottery :: Worker -> State -> (State, Set Assignment)
winLottery w State{..} = (state', droppedAssignments)
  where
    (droppedAssignments, remainingAssignments) =
      Set.partition (\a -> worker a == w) assignments
    state' = State
      { workers     = Set.delete w workers
      , assignments = remainingAssignments
      , ..
      }


instance Dhall.Interpret Task where
  autoWith a = Task <$> Dhall.autoWith a

instance Dhall.Interpret Worker where
  autoWith a = Worker <$> Dhall.autoWith a



