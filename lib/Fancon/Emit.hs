module Fancon.Emit (emit, pack) where

import Prelude hiding (Word)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.List (sortBy)
import Data.Bits (shiftL, (.|.), zeroBits)
import Data.Array

import Fancon.Instruction.Internal
import Fancon.Memory

emit :: Array Int Instruction -> B.ByteString
emit = B.toLazyByteString . mconcat . fmap emitInstruction . elems

emitInstruction :: Instruction -> B.Builder
emitInstruction ins@Instruction{operands} =
  emitFirstByte ins <> emitOperands (sortBy packingOrder operands)

emitFirstByte :: Instruction -> B.Builder
emitFirstByte (Instruction opcode operands) = pack opcodeByte layoutByte
  where opcodeByte = fromIntegral . fromEnum $ opcode
        layoutByte :: Byte
        layoutByte = foldr (.|.) zeroBits (layoutBit <$> operands)
        layoutBit (Register _) = 0b0
        layoutBit (Immediate _) = 0b1

packingOrder :: Operand -> Operand -> Ordering
packingOrder (Register _) _ = LT
packingOrder _ (Register _) = GT
packingOrder (Immediate _) _ = GT

emitOperands :: [Operand] -> B.Builder
emitOperands (Register a:Register b:rest) = pack a b <> emitOperands rest
emitOperands (Register a:rest) = B.word8 a <> emitOperands rest
emitOperands (Immediate a:rest) = B.word16BE a <> emitOperands rest
emitOperands [] = mempty

pack :: Byte -> Byte -> B.Builder
pack a b = B.word8 $ shiftL a 3 .|. b
