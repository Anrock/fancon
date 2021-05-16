{-# LANGUAGE NoOverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Fancon
import Fancon.Assemble hiding (Error, Warning)
import Fancon.Assemble qualified as Asm

import Fancon.Link hiding (Error, Warning)

import Data.Text (Text)
import Data.ByteString.Lazy qualified as B

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [ unit_InstructionsBinary
                                   , unit_Assembler
                                   , unit_Linker
                                   ]

unit_InstructionsBinary :: TestTree
unit_InstructionsBinary = testGroup "Instructions binary"
  [ testCase "add r0 r0 r0 is all zeroes" $ do
      compileTest "add r0 r0 r0" @?= B.pack [0, 0, 0]

  , testCase "registers are packed in nibbles" $ do
      compileTest "add r1 r1 r0" @?= B.pack [0, 0b00010001, 0]
      compileTest "add r15 r15 r0" @?= B.pack [0, 0b11111111, 0]
  ]

unit_Assembler :: TestTree
unit_Assembler = testGroup "Assembly" [ unit_AssemblerWarnings, unit_AssemblerErrors ]

unit_AssemblerWarnings :: TestTree
unit_AssemblerWarnings = testGroup "Warnings"
  [ testCase "warns about unknown commands" $ do
      warnings ".unknown"
        @?= [UnknownCommand "unknown" 1]
  , testCase "warns about unreferenced symbols" $ do
      warnings ".label unused\nadd r0 r0 r0"
        @?= [UnreferencedSymbol "unused" 1]
  , testCase "warns about unused imports" $ do
      warnings ".import unused\nadd r0 r0 r0"
        @?= [UnreferencedSymbol "unused" 1]
  ] where warnings t = let Right (w, _) = assembleTest t in w

unit_AssemblerErrors :: TestTree
unit_AssemblerErrors = testGroup "Errors"
  [ testCase "errors on duplicate symbols" $ do
      errors ".label foo\nadd r0 r0 r0\n.label foo\nadd r0 r0 r0"
        @?= [DuplicateSymbolDefinition "foo" 3]

  , testCase "errors on undefined symbol reference" $ do
      errors "add r0 undefined r0"
        @?= [UndefinedSymbolReference "undefined" 1]

  {--
  , testCase "errors on immediate out of bounds" $ do
      errors "add r0 67000 r0"
        @?= [InvalidWord "67000" 1]
  --}

  , testCase "errors on register index out of bounds" $ do
      errors "add r17 0 r0"
        @?= [InvalidOperands [Register 17, Immediate 0, Register 0] 1]

  , testCase "errors on invalid opcode" $ do
      errors "poo r0 r0 r0"
        @?= [InvalidOpcode "poo" 1]

  , testCase "errors on invalid operands" $ do
      errors "add r0 r0 42"
        @?= [InvalidOperands [Register 0, Register 0, Immediate 42] 1]

  , testCase "errors on import name collision" $ do
      errors ".import foo\n.label foo"
        @?= [DuplicateSymbolDefinition "foo" 2]

  , testCase "errors on undefined export" $ do
      errors ".export foo\nadd r0 r0 r0"
        @?= [UndefinedSymbolReference "foo" 1]

  , testCase "errors on undefined reference" $ do
      errors "add r0 foo r0"
        @?= [UndefinedSymbolReference "foo" 1]
  ] where errors t = let Left e = assembleTest t in e

unit_Linker :: TestTree
unit_Linker = testGroup "Linker" [ unit_LinkerWarnings, unit_LinkerErrors]

unit_LinkerWarnings :: TestTree
unit_LinkerWarnings = testGroup "Warnings"
  [ testCase "warns when there is no main symbol" $ do
      let m1 = "add r0 r0 r0"
          m2 = "add r0 r0 r0"
        in warnings [m1, m2] @?= [NoMain]

  , testCase "warns when there is unused symbol" $ do
      let m1 = ".export foo\n.label foo\nadd r0 r0 r0"
          m2 = ".export main\n.label main\nadd r0 r0 r0"
        in warnings [m1, m2] @?= [Unused "foo"]
  ] where warnings ms = let Right (ws, _) = link $ assembleModule <$> ms in ws

unit_LinkerErrors :: TestTree
unit_LinkerErrors = testGroup "Errors"
  [ testCase "errors when there is undefined symbol" $ do
      let m1 = ".import foo\nadd r0 foo r0"
          m2 = "add r0 r0 r0"
        in errors [m1, m2] @?= [Undefined "foo"]

  {-
  , testCase "errors when more than one symbol with same name" $ do
      let m1 = ".export foo\n.label foo\nadd r0 r0 r0"
          m2 = ".export foo\n.label foo\nadd r0 r0 r0"
        in errors [m1, m2] @?= [DuplicateDefinition "foo"]
  -}
  ] where errors ms = let Left es = link $ assembleModule <$> ms in es

assembleModule :: Text -> Module
assembleModule t = m
  where Right (_, m) = assembleTest t

assembleTest :: Text -> Either [Asm.Error] ([Asm.Warning], Module)
assembleTest ins = assembled
  where (Right parsed) = parse ins
        assembled = assemble parsed

compileTest :: Text -> B.ByteString
compileTest ins = binary
  where Right (_, (_, instructions)) = assembleTest ins
        binary = emit instructions
