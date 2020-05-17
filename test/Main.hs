module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Fancon
import Data.Array
import qualified Data.ByteString.Lazy as B

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [ unit_InstructionsBinary ]

unit_InstructionsBinary :: TestTree
unit_InstructionsBinary = testGroup "Instructions binary" [
    testCase "add r0 r0 r0 is all zeroes" $ do
      let (Right parsed) = parse "add r0 r0 r0"
          (_, Right (_, instructions)) = assemble parsed
          binary = emit instructions

      binary @?= B.pack [0, 0, 0]
  ]

