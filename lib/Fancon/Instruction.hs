module Fancon.Instruction
  ( Opcode(..)
  , Operand(..)
  , Instruction
  , opcode
  , operands
  , printInstructions
  , arity
  , maxArity
  , module Fancon.Instruction.Validate
  ) where

import Fancon.Instruction.Internal
import Fancon.Instruction.Validate
import Data.Array
import Data.String.Interpolate
import Data.List (intercalate)

printInstructions :: Array Int Instruction -> String
printInstructions instructions = [i|#{length instructions} instructions:\n#{intercalate "\n" $ show <$> assocs instructions}|]
