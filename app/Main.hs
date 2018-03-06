module Main where

import System.Environment
import System.IO
import System.Random (mkStdGen)

import Data.Aeson (encode)

import qualified Data.ByteString.Lazy as ByteString

import Data.Maze

main :: IO ()
main = do
  args <- getArgs
  case args of
    [first, second] |
      [(depth, _)] <- reads first,
      [(features, _)] <- reads second -> do
      let generator = mkStdGen 538213
      let strategy = toStrategy Consistent [0] [1]
      let maze = generateMaze generator strategy features depth
      ByteString.putStr $ encode maze
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "Usage: " ++ name ++ " <depth> <number of features>"
