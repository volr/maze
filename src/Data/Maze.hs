{-# LANGUAGE DeriveGeneric #-}

-- \ A T-Maze generator and evaluator for use in machine learning systems
module Data.Maze (
  contains, find, generateMaze, toStrategy, walk,
  Direction(Left, Right),
  Features,
  Maze(Blind, Exit, Entry, Fork),
  MazePath(MazePath, node, features),
  FeatureStrategy(FeatureStrategy),
  FeatureGenerationStrategy(Consistent)
) where

import Prelude hiding (Either(Left, Right))

import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics

import Data.Aeson
import Data.Aeson.TH

import System.Random

-- | The only two directions an actor can move
data Direction = Left | Right deriving (Generic, Show)
instance FromJSON Direction
instance ToJSON Direction

-- | Features in the maze which will appear at each 'Fork' (choices)
type Features = [Bool]

-- | The four possible types of nodes featured in a T-maze
data Maze = Blind -- ^ A blind path
          | Entry  -- ^ The maze entry
          | Exit  -- ^ The maze exit
          -- | A fork which gives the actor a choice: left or right
          | Fork { left :: MazePath
                 , right :: MazePath
                 }
          deriving (Eq, Generic, Show)

instance FromJSON Maze
instance ToJSON Maze

-- | A node of the maze containing a path to a new node and the features
--   associated with the path
data MazePath = MazePath {
  node :: Maze
, features :: Features
} deriving (Eq, Generic, Show)

instance FromJSON MazePath
instance ToJSON MazePath

-- | A strategy for generating features
data FeatureStrategy = FeatureStrategy {
  blind :: FeatureGenerationStrategy
, exit :: FeatureGenerationStrategy
}

-- | Strategies for rendering features in situations where the actor
--   will have to make a choice
newtype FeatureGenerationStrategy
  -- | Consistently turns on the feature with the given indices
  = Consistent (Set Int)
  -- | Randomly turns on features with a certain probability 0 < t < 1
  -- Random Float

-- | Tests whether a node is contained in a given 'Maze'
contains
  :: Maze -- ^ The node to search for
  -> Maze -- ^ The maze to search through
  -> Bool -- ^ True if the node is contained in the Maze
contains node = find (== node)

-- | Searches for at least one element fulfilling the predicate in a maze
find :: (Maze -> Bool) -> Maze -> Bool
find f maze@(Fork mazeLeft mazeRight) =
  f maze || find f (node mazeLeft) || find f (node mazeRight)
find f maze = f maze

-- | Generates a 'Maze' at a certain depth with the given 'FeatureStrategy' for
--   displaying features for each 'Fork' in the maze
generateMaze
  :: (RandomGen generator) =>
  generator ->       -- ^ The random generator to use when branching and displaying features
  FeatureStrategy -> -- ^ Instructions on how to display features for each junction ('Fork')
  Int ->             -- ^ The number of features in the maze (length of feature vector)
  Int ->             -- ^ The depth of the maze (number of junctions)
  Maze               -- ^ A 'Maze' that contains at least one 'Exit'
generateMaze _ _ _ 0 = Exit
generateMaze generator strategy features depth =
  let (isLeftExit, generatorBranch) = random generator
      branchBlind = generateBlindMaze (blind strategy) features (depth - 1)
      branchExit =
        MazePath (generateMaze generatorBranch strategy features (depth - 1))
                 (generateFeatures (exit strategy) features)
  in if isLeftExit
    then Fork branchExit branchBlind
    else Fork branchBlind branchExit

generateBlindMaze :: FeatureGenerationStrategy -> Int -> Int -> MazePath
generateBlindMaze strategy features 0 = MazePath Blind $ generateFeatures strategy features
generateBlindMaze strategy features depth =
  MazePath (Fork (generateBlindMaze strategy features (depth - 1))
                 (generateBlindMaze strategy features (depth - 1)))
           (generateFeatures strategy features)

generateFeatures :: FeatureGenerationStrategy -> Int -> Features
generateFeatures strategy features =
  generateFeaturesRecursive features strategy features

-- | Generates features considering the generation strategy one index at a time
generateFeaturesRecursive :: Int -> FeatureGenerationStrategy -> Int -> Features
generateFeaturesRecursive 0 _ _ = []
generateFeaturesRecursive iterations (Consistent set) features =
  Set.member (features - iterations) set :
    generateFeaturesRecursive (iterations - 1) (Consistent set) features

-- | Creates a 'FeatureStrategy' given a constructor and to lists of indices
toStrategy :: (Set Int -> FeatureGenerationStrategy) -> [Int] -> [Int] -> FeatureStrategy
toStrategy f blind exit = FeatureStrategy (f (Set.fromList blind)) (f (Set.fromList exit))

walk :: [Direction] -> Maze -> Maze
walk (Left:directions) (Fork left right) = walk directions (node left)
walk (Right:directions) (Fork left right) = walk directions (node right)
walk _ maze = maze
