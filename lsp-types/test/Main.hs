module Main where

import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = hspec Spec.spec
