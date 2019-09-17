{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TaskTree where

import qualified Algebra.Graph   as G
import qualified Data.Aeson      as Aeson
import qualified Data.List       as List
import           Data.List       (partition)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Dhall           as Dhall
import           GHC.Generics    (Generic)
import qualified Web.HttpApiData as HttpApiData
import qualified Servant.Docs    as Servant
import           Servant.API     (Capture(..), QueryParam(..))

newtype Task = Task {
  unTask :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Task
instance Aeson.FromJSON Task

instance HttpApiData.FromHttpApiData Task where
  parseUrlPiece t = Task <$> HttpApiData.parseUrlPiece t

instance HttpApiData.ToHttpApiData Task where
  toUrlPiece (Task t) = HttpApiData.toUrlPiece t

instance Dhall.Interpret Task where
  autoWith a = Task <$> Dhall.autoWith a


newtype Worker = Worker {
  unWorker :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Worker
instance Aeson.FromJSON Worker

instance Dhall.Interpret Worker where
  autoWith a = Worker <$> Dhall.autoWith a

instance HttpApiData.FromHttpApiData Worker where
  parseUrlPiece t = Worker <$> HttpApiData.parseUrlPiece t

instance HttpApiData.ToHttpApiData Worker where
  toUrlPiece (Worker t) = HttpApiData.toUrlPiece t


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

instance Servant.ToSample State where
  toSamples _ = Servant.singleSample $ State
    { tasks = G.edges [(Task "Product", Task "Intermediate")
                      ,(Task "Intermediate", Task "Parts")
                      ]
    , workers = Set.fromList $ Worker <$> ["Alp", "Julian", "Andres"]
    , assignments = Set.fromList [ Assignment (Task "Intermediate") (Worker "Andres")]
    }


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

--------------------------------------------------------------------------------
assignmentTree :: State -> G.Graph (Task, Maybe Worker)
assignmentTree State{tasks, assignments} =
  List.foldl' doAssignment graph0 asgns
  where
    asgns  = Set.toList assignments
    graph0 = fmap (\task -> (task, Nothing)) tasks
    doAssignment g Assignment{task, worker} =
      G.replaceVertex (task, Nothing) (task, Just worker) g


--------------------------------------------------------------------------------
-- Definitions needed for servant-docs
--------------------------------------------------------------------------------


instance Servant.ToCapture (Capture "task" Task) where
  toCapture _ = Servant.DocCapture "task" "Name of a task"

instance Servant.ToParam (QueryParam "task" Task) where
  toParam _ =
    Servant.DocQueryParam "task"
    ["Bug 15", "Pin", "..."]
    "Name of the task."
    Servant.Normal -- Normal, List or Flag

instance Servant.ToCapture (Capture "worker" Worker) where
  toCapture _ = Servant.DocCapture "worker" "Name of a worker"

instance Servant.ToParam (QueryParam "worker" Worker) where
  toParam _ =
    Servant.DocQueryParam "worker"
    ["Alp", "Julian", "..."]
    "Name of the worker."
    Servant.Normal -- Normal, List or Flag
