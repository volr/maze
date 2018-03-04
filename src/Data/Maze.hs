-- \ A T-Maze generator and executor for use in online learning systems
module Data.Maze (generateMaze, Direction(Left, Right, Back), MazeNode(Exit, Entry)) where

import System.Random

data Direction = Left | Right | Back deriving Show

data MazeNode a = Blind | Entry | Exit
                | Fork { left :: MazeNode a, right :: MazeNode a }
                deriving (Eq, Show)

generateMaze :: Integer -> MazeNode Integer
generateMaze 0 = Exit
generateMaze n = generateBlindMaze n

generateBlindMaze :: Integer -> MazeNode Integer
generateBlindMaze 0 = Blind
generateBlindMaze n = Fork (generateBlindMaze (n - 1)) (generateBlindMaze (n - 1))
