-- \ A T-Maze generator and evaluator for use in machine learning systems
module Data.Maze (
  contains, generateMaze,
  Direction(Left, Right, Back),
  Features,
  MazeNode(Blind, Exit, Entry, Fork),
  FeatureStrategy(Exclusive)
) where

import System.Random

-- | The only three directions an actor can move
data Direction = Left | Right | Back deriving Show

-- | Features in the maze which will appear at each 'Fork' (choices)
type Features = [Bool]

-- | The four possible types of nodes featured in a T-maze
data MazeNode a = Blind -- ^ A blind path
                | Entry  -- ^ The maze entry
                | Exit  -- ^ The maze exit
                -- | A fork which gives the actor a choice: left or right
                | Fork { left :: MazeNode a, right :: MazeNode a }
                deriving (Eq, Show)

-- | Strategies for rendering features in situations where the actor
--   will have to make a choice
newtype FeatureStrategy =
  -- | Exclusively use the feature with the given index to lead
  --   towards the exit of the maze
  Exclusive Integer

-- | Tests whether a maze contains a given 'MazeNode'
contains :: MazeNode a -> MazeNode a -> Bool
contains (Fork mazeLeft mazeRight) node =
  contains mazeLeft node || contains mazeRight node
contains a b = a == b

generateMaze :: (RandomGen generator) => generator -> FeatureStrategy -> Integer -> Integer -> MazeNode Integer
generateMaze _ _ _ 0 = Exit
generateMaze generator strategy features depth =
  let (isLeftExit, generatorBranch) = random generator
      branchBlind = generateBlindMaze (depth - 1)
      branchExit = generateMaze generatorBranch strategy features (depth - 1)
  in if isLeftExit
    then Fork branchExit branchBlind
    else Fork branchBlind branchExit

generateBlindMaze :: Integer -> MazeNode Integer
generateBlindMaze 0 = Blind
generateBlindMaze n = Fork (generateBlindMaze (n - 1)) (generateBlindMaze (n - 1))
