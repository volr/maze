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
      contains (Fork (MazePath Exit []) (MazePath Blind [])) Blind `shouldBe` True
      contains (Fork (MazePath Exit []) (MazePath Exit [])) Blind `shouldBe` False

    it "can generate an empty maze" $
      generateMaze gen (Exclusive 0) 0 0 `shouldBe` Exit

    it "can generate a maze with one level" $ do
      let maze = generateMaze gen (Exclusive 0) 0 1
      contains maze Blind `shouldBe` True
      contains maze Exit `shouldBe` True

    --it "can show the correct feature on the exit"
