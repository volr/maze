-- \ A T-Maze generator and evaluator for use in machine learning systems
module Data.Maze (
  contains, find, generateMaze,
  Direction(Left, Right, Back),
  Features,
  Maze(Blind, Exit, Entry, Fork),
  MazePath(MazePath),
  FeatureStrategy(Exclusive)
) where

import System.Random

-- | The only three directions an actor can move
data Direction = Left | Right | Back deriving Show

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
          deriving (Eq, Show)

-- | A node of the maze containing a path to a new node and the features
--   associated with the path
data MazePath = MazePath {
  node :: Maze
, features :: Features
} deriving (Eq, Show)

-- | Strategies for rendering features in situations where the actor
--   will have to make a choice
newtype FeatureStrategy =
  -- | Exclusively use the feature with the given index to lead
  --   towards the exit of the maze
  Exclusive Int

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

generateMaze :: (RandomGen generator) => generator -> FeatureStrategy -> Int -> Int -> Maze
generateMaze _ _ _ 0 = Exit
generateMaze generator strategy features depth =
  let (isLeftExit, generatorBranch) = random generator
      branchBlind = generateBlindMaze strategy features (depth - 1)
      branchExit =
        MazePath (generateMaze generatorBranch strategy features (depth - 1))
                 (replicate features False)
  in if isLeftExit
    then Fork branchExit branchBlind
    else Fork branchBlind branchExit

generateBlindMaze :: FeatureStrategy -> Int -> Int -> MazePath
generateBlindMaze strategy features 0 = MazePath Blind $ generateBlindFeatures strategy features
generateBlindMaze strategy features depth =
  MazePath (Fork (generateBlindMaze strategy features (depth - 1))
                 (generateBlindMaze strategy features (depth - 1)))
           (generateBlindFeatures strategy features)

generateBlindFeatures :: FeatureStrategy -> Int -> Features
generateBlindFeatures (Exclusive n) features =
  let (head,tail) = splitAt n $ replicate (features - 1) False
  in head ++ [True] ++ tail

--generateExitFeatures :: FeatureStrategy -> Integer -> Features
