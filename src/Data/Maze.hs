-- \ A T-Maze generator and executor for use in online learning systems
module Data.Maze (generateMaze, Direction, MazeNode(Exit, Entry)) where

data Direction = Left | Right | Back deriving Show

data MazeNode a = Blind | Entry | Exit
                | Fork { left :: a, right :: a, parent :: MazeNode a }
                deriving (Eq)

instance (Show a) => Show (MazeNode a) where
  show Blind = "Blind"
  show Entry = "Entry"
  show Exit = "Exit"
  show (Fork left right _) = "Fork " ++ (show left) ++ " " ++ (show right)

generateMaze :: Integer -> MazeNode Integer
generateMaze 0 = Exit
generateMaze n = Exit
