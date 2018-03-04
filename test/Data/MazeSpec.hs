module Data.MazeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Maze

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "a maze" $ do
    it "can generate an empty maze" $ do
      generateMaze 0 `shouldBe` Exit
