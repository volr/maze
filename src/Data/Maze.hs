-- \ A T-Maze generator and evaluator for use in machine learning systems
module Data.Maze (
  contains, generateMaze,
  Direction(Left, Right, Back),
  MazeNode(Blind, Exit, Entry, Fork)
) where


import Data.List
import System.Random

-- | The only three directions an actor can move
data Direction = Left | Right | Back deriving Show

-- | Features in the maze which will appear at each fork (choice)
--type Features a = List a

-- | The four possible types of nodes featured in a T-maze
data MazeNode a = Blind -- ^ A blind path
                | Entry  -- ^ The maze entry
                | Exit  -- ^ The maze exit
                -- | A fork which gives the actor a choice: left or right
                | Fork { left :: MazeNode a, right :: MazeNode a }
                deriving (Eq, Show)

-- | Strategies for
newtype FeatureStrategy = Consistent Integer

-- | Tests whether a maze contains a given node
contains :: MazeNode a -> MazeNode a -> Bool
contains (Fork mazeLeft mazeRight) node =
  contains mazeLeft node || contains mazeRight node
contains a b = a == b

generateMaze :: (RandomGen g) => g -> Integer -> MazeNode Integer
generateMaze _ 0 = Exit
generateMaze g n =
  let (isLeftExit, genBranch) = random g
      branchBlind = generateBlindMaze (n - 1)
      branchExit = generateMaze genBranch (n - 1)
  in if isLeftExit
    then Fork branchExit branchBlind
    else Fork branchBlind branchExit

generateBlindMaze :: Integer -> MazeNode Integer
generateBlindMaze 0 = Blind
generateBlindMaze n = Fork (generateBlindMaze (n - 1)) (generateBlindMaze (n - 1))
