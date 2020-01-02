module Fancon.Instruction
  ( Opcode(..)
  , OperandsLayout
  , OperandType(..)
  , Operands(..)
  , Instruction(..)
  , Valid
  , unValid
  , validate
  , Error(..)
  ) where

import Prelude hiding (div, and, or, Word)
import Control.Applicative ((<|>))

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
            deriving (Eq, Show, Bounded, Enum)

data OperandType = Immediate | Register | Unused deriving (Eq, Show)

type OperandsLayout = (OperandType, OperandType, OperandType)

data Operands = RRR Byte Byte Byte
              | RRI Byte Byte Word
              | RII Byte Word Word
              | III Word Word Word
              | RR Byte Byte
              | RI Byte Word
              | II Word Word
              | R Byte
              | I Word
              | None
              deriving (Eq, Show)

data Instruction = Instruction Opcode OperandsLayout Operands deriving (Eq, Show)

newtype Valid = Valid { unValid :: Instruction } deriving (Eq, Show)

data Error = InvalidLayout
           | InvalidOperands
  deriving (Eq, Show)

validate :: Instruction -> Either Error Valid
validate ins@(Instruction opcode layout operands) =
  case validateLayout opcode layout >> validateOperands layout operands of
    Just e -> Left e
    Nothing -> Right . Valid $ ins

validateLayout :: Opcode -> OperandsLayout -> Maybe Error
validateLayout Add (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Sub (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Div (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Mul (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Xor (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Shf (a, b, c) = isRI a >> isRI b >> isR c
validateLayout And (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Or  (a, b, c) = isRI a >> isRI b >> isR c
validateLayout Save (a, b, c) = isRI a >> isRI b >> isU c
validateLayout Load (a, b, c) = isRI a >> isR b >> isU c
validateLayout Jgz (a, b, c) = isR a >> isRI b >> isU c
validateLayout Jlt (a, b, c) = isR a >> isRI b >> isU c
validateLayout Jez (a, b, c) = isR a >> isRI b >> isU c
validateLayout Int (a, b, c) = isU a >> isU b >> isU c
validateLayout Brk (a, b, c) = isU a >> isU b >> isU c

isR, isI, isRI, isU :: OperandType -> Maybe Error
isR Register = Nothing
isR _ = Just InvalidLayout
isI Immediate = Nothing
isI _ = Just InvalidLayout
isU Unused = Nothing
isU _ = Just InvalidLayout
isRI o = isI o <|> isR o

validateOperands :: OperandsLayout -> Operands -> Maybe Error
validateOperands (Register, Register, Register) RRR {} = Nothing

validateOperands (Register, Immediate, Register) RRI {} = Nothing
validateOperands (Register, Register, Immediate) RRI {} = Nothing
validateOperands (Immediate, Register, Register) RRI {} = Nothing

validateOperands (Register, Immediate, Immediate) RII {} = Nothing
validateOperands (Immediate, Register, Immediate) RII {} = Nothing
validateOperands (Immediate, Immediate, Register) RII {} = Nothing

validateOperands (Immediate, Immediate, Immediate) III {} = Nothing

validateOperands (Register, Register, Unused) RR {} = Nothing
validateOperands (Register, Unused, Register) RR {} = Nothing
validateOperands (Unused, Register, Register) RR {} = Nothing

validateOperands (Register, Immediate, Unused) RI {} = Nothing
validateOperands (Register, Unused, Immediate) RI {} = Nothing
validateOperands (Immediate, Register, Unused) RI {} = Nothing
validateOperands (Immediate, Unused, Register) RI {} = Nothing
validateOperands (Unused, Register, Immediate) RI {} = Nothing
validateOperands (Unused, Immediate, Register) RI {} = Nothing

validateOperands (Immediate, Immediate, Unused) II {} = Nothing
validateOperands (Immediate, Unused, Immediate) II {} = Nothing
validateOperands (Unused, Immediate, Immediate) II {} = Nothing

validateOperands (Register, Unused, Unused) R {} = Nothing
validateOperands (Unused, Register, Unused) R {} = Nothing
validateOperands (Unused, Unused, Register) R {} = Nothing

validateOperands (Immediate, Unused, Unused) I {} = Nothing
validateOperands (Unused, Immediate, Unused) I {} = Nothing
validateOperands (Unused, Unused, Immediate) I {} = Nothing

validateOperands (Unused, Unused, Unused) None = Nothing

validateOperands _ _ = Just InvalidOperands
