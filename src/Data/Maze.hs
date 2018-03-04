module Data.Maze (generateMaze) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type Features = [Integer]

data Direction = Left | Right | Back deriving Show
data MazeNode = Fork Features Features | Blind | Exit

generateMaze :: Integer -> Gr Char ()
generateMaze 0 = mkGraph [] []
generateMaze n = mkGraph (zip [1..2] "ab") []
