module Main where

import Test.Hspec.Runner
import qualified Spec

main :: IO ()
main = hspec Spec.spec
