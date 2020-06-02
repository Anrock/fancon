module Fancon.Instruction.Validate
  ( validateOpcode
  , validateInstruction
  ) where

import Data.Text (Text, unpack, toTitle)
import Text.Read (readMaybe)

import Fancon.Instruction.Internal

validateOpcode :: Text -> Maybe Opcode
validateOpcode = readMaybe . unpack . toTitle

validateInstruction :: Opcode -> [Operand] -> Maybe Instruction
validateInstruction opcode operands = if validOperandsForOpcode opcode operands
                                      then Just $ Instruction opcode operands
                                      else Nothing

validOperandsForOpcode :: Opcode -> [Operand] -> Bool
validOperandsForOpcode Add = validateArithOperands
validOperandsForOpcode Sub = validateArithOperands
validOperandsForOpcode Div = validateArithOperands
validOperandsForOpcode Mul = validateArithOperands
validOperandsForOpcode Xor = validateArithOperands
validOperandsForOpcode Shf = validateArithOperands
validOperandsForOpcode And = validateArithOperands
validOperandsForOpcode Or = validateArithOperands
validOperandsForOpcode Saveh = validateMemoryOperands
validOperandsForOpcode Savehw = validateMemoryOperands
validOperandsForOpcode Save = validateMemoryOperands
validOperandsForOpcode Savew = validateMemoryOperands
validOperandsForOpcode Loadh = validateMemoryOperands
validOperandsForOpcode Loadhw = validateMemoryOperands
validOperandsForOpcode Load = validateMemoryOperands
validOperandsForOpcode Loadw = validateMemoryOperands
validOperandsForOpcode Jgz = validateJumpOperands
validOperandsForOpcode Jlt = validateJumpOperands
validOperandsForOpcode Jez = validateJumpOperands
validOperandsForOpcode Int = validateSpecialOperands
validOperandsForOpcode Hlt = validateSpecialOperands

validateArithOperands :: [Operand] -> Bool
validateArithOperands [a, b, c] = isR a && isRI b && isR c
validateArithOperands _ = False

validateMemoryOperands :: [Operand] -> Bool
validateMemoryOperands [a, b] = isRI a && isRI b
validateMemoryOperands _ = False

validateJumpOperands :: [Operand] -> Bool
validateJumpOperands [a, b] = isR a && isRI b
validateJumpOperands _ = False

validateSpecialOperands :: [Operand] -> Bool
validateSpecialOperands = null

isR, isI, isRI :: Operand -> Bool

isR (Register r) | r >= 0 && r <= 15 = True
isR _ = False

isI (Immediate i) | i >= 0 && i <= 65535 = True
isI _ = False

isRI o = isI o || isR o
