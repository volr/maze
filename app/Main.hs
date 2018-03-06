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
    ["generate", first, second]
      | [(depth, _)] <- reads first
      , [(features, _)] <- reads second -> do
      let generator = mkStdGen 538213
      let strategy = toStrategy Consistent [0] [1]
      let maze = generateMaze generator strategy features depth
      ByteString.putStr $ encode maze
    ["walk", choices, mazeString]
      | Just maze <- (decode (ByteString.Char8.pack mazeString) :: Maybe Maze) ->
      putStrLn $ show maze
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ unlines
        [ "Usage: " ++ name ++ " generate <depth> <number of features>"
        , "       " ++ name ++ " walk <list of choices> <maze>"
        ]
