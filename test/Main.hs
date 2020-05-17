module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Fancon
import Data.Array
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [ unit_InstructionsBinary ]

unit_InstructionsBinary :: TestTree
unit_InstructionsBinary = testGroup "Instructions binary"
  [ testCase "add r0 r0 r0 is all zeroes" $ do
      compile "add r0 r0 r0" @?= B.pack [0, 0, 0]

  , testCase "registers are packed in nibbles" $ do
      compile "add r1 r1 r0" @?= B.pack [0, 0b00010001, 0]
      compile "add r15 r15 r0" @?= B.pack [0, 0b11111111, 0] 
  ]

compile :: Text -> B.ByteString
compile ins = binary
  where (Right parsed) = parse ins
        (_, Right (_, instructions)) = assemble parsed
        binary = emit instructions
