module Fancon.Emit (emit) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.List (sortBy)
import Data.Bits (shiftL, (.|.), zeroBits)
import Data.Word (Word8)

import Fancon.Instruction.Internal

emit :: Foldable f => f Instruction -> B.ByteString
emit = B.toLazyByteString . foldMap emitInstruction

emitInstruction :: Instruction -> B.Builder
emitInstruction ins@Instruction{operands} =
  emitDescriptionByte ins <> emitOperands (sortBy packingOrder operands)

emitDescriptionByte :: Instruction -> B.Builder
emitDescriptionByte i = B.word8 $ zeroBits .|. typeBits (opcode i) .|. typeSpecificBits i

typeBits :: Opcode -> Word8
typeBits opcode =
  let aType = 0b00000000
      mType = 0b01000000
      jType = 0b10000000
      sType = 0b11000000
   in case opcode of
     Add -> aType
     Sub -> aType
     Div -> aType
     Mul -> aType
     Xor -> aType
     Shf -> aType
     And -> aType
     Or -> aType
     Saveh -> mType
     Savehw -> mType
     Save -> mType
     Savew -> mType
     Loadh -> mType
     Loadhw -> mType
     Load -> mType
     Loadw -> mType
     Jgz -> jType
     Jlt -> jType
     Jez -> jType
     Int -> sType
     Hlt -> sType

isImmediate :: Operand -> Bool
isImmediate Immediate {} = True
isImmediate _ = False

aImmediateBit :: [Operand] -> Word8
aImmediateBit operands
  | isImmediate (head operands) = 0b00000010
  | otherwise = 0b0

bImmediateBit :: [Operand] -> Word8
bImmediateBit operands
  | isImmediate (operands !! 1) = 0b00000001
  | otherwise = 0b0

typeSpecificBits :: Instruction -> Word8
typeSpecificBits (Instruction opcode operands) =
  let aImmediate = aImmediateBit operands
      bImmediate = bImmediateBit operands
   in case opcode of
        Add    -> 0b00_0000_00 .|. bImmediate
        Sub    -> 0b00_0010_00 .|. bImmediate
        Div    -> 0b00_0100_00 .|. bImmediate
        Mul    -> 0b00_0110_00 .|. bImmediate
        Xor    -> 0b00_1000_00 .|. bImmediate
        Shf    -> 0b00_1010_00 .|. bImmediate
        And    -> 0b00_1100_00 .|. bImmediate
        Or     -> 0b00_1110_00 .|. bImmediate
        Saveh  -> 0b00_1100_00 .|. aImmediate .|. bImmediate
        Savehw -> 0b00_1110_00 .|. aImmediate .|. bImmediate
        Save   -> 0b00_1000_00 .|. aImmediate .|. bImmediate
        Savew  -> 0b00_1010_00 .|. aImmediate .|. bImmediate
        Loadh  -> 0b00_0100_00 .|. bImmediate
        Loadhw -> 0b00_0110_00 .|. bImmediate
        Load   -> 0b00_0000_00 .|. bImmediate
        Loadw  -> 0b00_0010_00 .|. bImmediate
        Jgz    -> 0b10_0000_00 .|. aImmediate .|. bImmediate
        Jlt    -> 0b10_0100_00 .|. aImmediate .|. bImmediate
        Jez    -> 0b10_1000_00 .|. aImmediate .|. bImmediate
        Int    -> 0b00_00_0000
        Hlt   -> 0b00_11_0000

packingOrder :: Operand -> Operand -> Ordering
packingOrder (Register _) _ = LT
packingOrder _ (Register _) = GT
packingOrder (Immediate _) _ = GT

emitOperands :: [Operand] -> B.Builder
emitOperands (Register a:Register b:rest) = packNibbles (fromIntegral a) (fromIntegral b) <> emitOperands rest
emitOperands (Register a:rest) = B.word8 (fromIntegral a) <> emitOperands rest
emitOperands (Immediate a:rest) = B.word16BE (fromIntegral a) <> emitOperands rest
emitOperands [] = mempty

packNibbles :: Word8 -> Word8 -> B.Builder
packNibbles a b = B.word8 $ shiftL a 4 .|. b
