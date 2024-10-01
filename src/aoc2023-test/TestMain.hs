module TestMain (main) where

import Ante.Test

import Spec qualified

main :: IO ()
main = hspec Spec.spec
