module Ante.Test
  ( module Ante.Test
  , module Ante
  , module Aoc2023.Solution
  , module Debug.Trace
  , module System.Environment
  , module Test.Hspec
  ) where

import Ante hiding (Arg)

import Aoc2023.Solution

import Data.ByteString.Char8 qualified as C8
import Debug.Trace
import GHC.IO.Handle
import System.Environment
import System.IO.Silently
import System.Process
import Test.Hspec

examples :: (Eq b, Show b) => String -> (a -> IO b) -> [(a, b)] -> Spec
examples lbl f exs =
  describe ("(examples) " <> lbl) do
    it "satisfies examples" do
      for_ exs \(x, y) -> (`shouldBe` y) =<< f x

blackbox :: String -> (Handle -> Handle -> IO ()) -> ByteString -> ByteString -> Spec
blackbox lbl fn fullInput expectedOutput =
  describe ("(blackbox) " <> lbl) do
    it ("produces expected output: " <> C8.unpack expectedOutput) do
      (inputR, inputW) <- createPipe
      (outputRead, outputWrite) <- createPipe
      C8.hPutStrLn inputW fullInput
      hClose inputW
      fn inputR outputWrite
      hClose outputWrite
      hClose inputR
      result <- C8.hGetContents outputRead
      hClose outputRead
      result `shouldBe` expectedOutput

dressRehersal :: Problem -> ByteString -> Spec
dressRehersal prob expectedOutput =
  describe ("(dressRehersal) " <> show prob) do
    it ("produces expected output: " <> C8.unpack expectedOutput) do
      output <- capture_ $ runSolution prob
      output `shouldBe` C8.unpack expectedOutput
