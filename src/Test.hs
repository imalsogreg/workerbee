{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test where

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Algebra.Graph as G
import           TaskTree
-- import           Test.HUnit    hiding (State)
import           Test.Hspec

main :: IO ()
main = hspec $ do

  describe "assign" $ do

    it "state conforms to manual test" $ example $ do
      let simpleState =
            State
            { workers = Set.fromList [greg, james]
            , tasks   = G.connect (G.vertex shipping) (G.vertex hacking)
            , assignments = Set.fromList
                            [Assignment { task = hacking, worker = greg }
                            ,Assignment {task = shipping, worker = james}
                            ]
            }        
      
      let (state', leftovers) = assign (Assignment hacking james) simpleState

      assignments state' `shouldBe` Set.singleton (Assignment hacking james)
      leftovers          `shouldBe` assignments simpleState



-- | Fixtures

greg  = Worker "Greg"
james = Worker "James"

hacking  = Task "hacking"
shipping = Task "shipping"

simpleState :: State
simpleState = State
     { workers = Set.fromList [greg, james]
     , tasks   = G.connect (G.vertex shipping) (G.vertex hacking)
     , assignments = Set.fromList
         [Assignment { task = hacking, worker = greg }
         ,Assignment {task = shipping, worker = james}
         ]
     }
