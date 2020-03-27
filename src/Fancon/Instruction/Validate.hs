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
validOperandsForOpcode Add _ = False
validOperandsForOpcode Sub [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Sub _ = False
validOperandsForOpcode Div [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Div _ = False
validOperandsForOpcode Mul [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Mul _ = False
validOperandsForOpcode Xor [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Xor _ = False
validOperandsForOpcode Shf [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Shf _ = False
validOperandsForOpcode And [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode And _ = False
validOperandsForOpcode Or  [a, b, c] = isRI a && isRI b && isR c
validOperandsForOpcode Or _ = False
validOperandsForOpcode Saveh [a, b] = isRI a && isRI b
validOperandsForOpcode Saveh _ = False
validOperandsForOpcode Save [a, b] = isRI a && isRI b
validOperandsForOpcode Save _ = False
validOperandsForOpcode Loadh [a, b] = isRI a && isR b
validOperandsForOpcode Loadh _ = False
validOperandsForOpcode Load [a, b] = isRI a && isR b
validOperandsForOpcode Load _ = False
validOperandsForOpcode Jgz [a, b] = isR a && isRI b
validOperandsForOpcode Jgz _ = False
validOperandsForOpcode Jlt [a, b] = isR a && isRI b
validOperandsForOpcode Jlt _ = False
validOperandsForOpcode Jez [a, b] = isR a && isRI b
validOperandsForOpcode Jez _ = False
validOperandsForOpcode Int [] = True
validOperandsForOpcode Int _ = False
validOperandsForOpcode Brk [] = True
validOperandsForOpcode Brk _ = False

isR, isI, isRI :: Operand -> Bool
isR Register {} = True
isR _ = False
isI Immediate {} = True
isI _ = False
isRI o = isI o || isR o
