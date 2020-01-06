module Fancon.Instruction.Internal
  ( Opcode(..)
  , Instruction(..)
  , Operand(..)
  ) where

import Prelude hiding (div, and, or, Word)

import Fancon.Memory

data Opcode = Add
            | Sub
            | Div
            | Mul
            | Xor
            | Shf
            | And
            | Or
            | Save
            | Load
            | Jgz
            | Jlt
            | Jez
            | Int
            | Brk
            deriving (Eq, Show, Bounded, Enum, Read)

data Operand = Register Byte | Immediate Word deriving (Eq, Show)

data Instruction = Instruction { opcode :: Opcode
                               , operands :: [Operand]
                               } deriving (Eq, Show)
