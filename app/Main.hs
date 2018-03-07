module Main where

import System.Environment
import System.IO
import System.Random (mkStdGen)

import Data.Aeson (decode, encode)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8

import Data.Maze

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["generate", depthString, blindString, exitString, featureString]
      | [(depth, _)] <- reads depthString
      , [(blindFeatures, _)] <- reads blindString
      , [(exitFeatures, _)] <- reads exitString
      , [(features, _)] <- reads featureString -> do
      let generator = mkStdGen 538213
      let strategy = toStrategy Consistent blindFeatures exitFeatures
      let maze = generateMaze generator strategy features depth
      ByteString.putStr $ encode maze
    ["walk", choiceString, mazeString]
      | Just choices <- (decode (ByteString.Char8.pack choiceString) :: Maybe [Direction])
      , Just maze <- (decode (ByteString.Char8.pack mazeString) :: Maybe Maze) ->
      case (walk choices maze) of
        Fork left right -> ByteString.putStr $ encode $ (features left, features right)
        node -> ByteString.putStr $ encode node
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ unlines
        [ "Usage: " ++ name ++ " generate <depth> <blind features> <exit features> <number of features>"
        , "       " ++ name ++ " walk <list of choices> <maze>"
        , "Examples: "
        , "   - " ++ name ++ " generate 2 [1] [0] 3"
        , "     Generates a maze of depth two where the feature in index 0 leads to the exit"
        , "   - " ++ name ++ " walk '[\"Right\"]' `walk generate 2 [1] [0] 3`"
        , "     Walks right in the maze above of depth 2"
        , "Author: Jens Egholm <jensegholm@protonmail.com>"
        ]
