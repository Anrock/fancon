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
import qualified Data.Vector as V
import Data.String.Interpolate
import Data.List (intercalate)

printInstructions :: V.Vector Instruction -> String
printInstructions instructions = [i|#{V.length instructions} instructions:\n#{intercalate "\n" $ show <$> V.toList instructions}|]
