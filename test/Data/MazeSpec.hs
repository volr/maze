module Data.MazeSpec (main, spec) where

import Test.Hspec
import qualified Test.QuickCheck

import System.Random (mkStdGen)

import Data.Maze as Maze

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let gen = mkStdGen 0

  describe "a maze" $ do

    it "can test if it contains a node" $ do
      contains Exit Exit `shouldBe` True
      contains Exit Entry `shouldBe` False
      contains Blind (Fork (MazePath Exit []) (MazePath Blind [])) `shouldBe` True
      contains Blind (Fork (MazePath Exit []) (MazePath Exit [])) `shouldBe` False

    it "can generate an empty maze" $
      generateMaze gen (Exclusive 0) 0 0 `shouldBe` Exit

    it "can generate a maze with one level" $ do
      let maze = generateMaze gen (Exclusive 0) 0 1
      contains Blind maze `shouldBe` True
      contains Exit maze `shouldBe` True

    it "can generate a maze with correct blind features" $ do
      let maze = generateMaze gen (Exclusive 1) 2 1
      maze `shouldBe` Fork (MazePath Exit [False, False]) (MazePath Blind [False, True])

    --it "can show the correct feature on the exit"
