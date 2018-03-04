-- \ A T-Maze generator and executor for use in online learning systems
module Data.Maze (
  contains, generateMaze,
  Direction(Left, Right, Back),
  MazeNode(Blind, Exit, Entry, Fork)
) where

import System.Random

data Direction = Left | Right | Back deriving Show

data MazeNode a = Blind | Entry | Exit
                | Fork { left :: MazeNode a, right :: MazeNode a }
                deriving (Eq, Show)

contains :: MazeNode a -> MazeNode a -> Bool
contains (Fork mazeLeft mazeRight) node =
  (contains mazeLeft node) || (contains mazeRight node)
contains a b = a == b

generateMaze :: (RandomGen g) => g -> Integer -> MazeNode Integer
generateMaze _ 0 = Exit
generateMaze g n =
  let (isLeftExit, genBranch) = random g
      branchBlind = generateBlindMaze (n - 1)
      branchExit = generateMaze genBranch (n - 1)
  in case isLeftExit of
    True -> Fork branchBlind branchExit
    False -> Fork branchExit branchBlind

generateBlindMaze :: Integer -> MazeNode Integer
generateBlindMaze 0 = Blind
generateBlindMaze n = Fork (generateBlindMaze (n - 1)) (generateBlindMaze (n - 1))
