module Fancon.Instruction.Internal
  ( Opcode(..)
  , Instruction(..)
  , Operand(..)
  , arity
  , maxArity
  ) where

import Prelude hiding (div, and, or)

import Fancon.Memory

data Opcode = Add
            | Sub
            | Div
            | Mul
            | Xor
            | Shf
            | And
            | Or
            | Saveh
            | Savehw
            | Save
            | Savew
            | Loadh
            | Loadhw
            | Load
            | Loadw
            | Jgz
            | Jlt
            | Jez
            | Int
            | Hlt
            deriving (Eq, Show, Bounded, Enum, Read)

data Operand = Register Int | Immediate Int deriving (Eq, Show)

data Instruction = Instruction { opcode :: Opcode
                               , operands :: [Operand]
                               } deriving (Eq, Show)

arity :: Opcode -> Int
arity Add    = 3
arity Sub    = 3
arity Div    = 3
arity Mul    = 3
arity Xor    = 3
arity Shf    = 3
arity And    = 3
arity Or     = 3
arity Saveh  = 2
arity Savehw = 2
arity Save   = 2
arity Savew  = 2
arity Loadh  = 2
arity Loadhw = 2
arity Load   = 2
arity Loadw  = 2
arity Jgz    = 2
arity Jlt    = 2
arity Jez    = 2
arity Int    = 0
arity Hlt   = 0

maxArity :: Int
maxArity = 3
