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
validOperandsForOpcode Add [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Sub [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Div [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Mul [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Xor [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Shf [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode And [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Or  [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Save [a, b] = isRI a && isRI b
validOperandsForOpcode Load [a, b] = isRI a && isR b
validOperandsForOpcode Jgz [a, b] = isR a && isRI b
validOperandsForOpcode Jlt [a, b] = isR a && isRI b
validOperandsForOpcode Jez [a, b] = isR a && isRI b
validOperandsForOpcode Int [] = True
validOperandsForOpcode Brk [] = True
validOperandsForOpcode _   _ = False

isR, isI, isRI :: Operand -> Bool
isR Register {} = True
isR _ = False
isI Immediate {} = True
isI _ = False
isRI o = isI o || isR o
