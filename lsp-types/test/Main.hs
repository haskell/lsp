module Main where

import qualified Spec
import           Test.Hspec.Runner

main :: IO ()
main = hspec Spec.spec
