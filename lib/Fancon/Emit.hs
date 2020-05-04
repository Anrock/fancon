module Fancon.Emit (emit) where

import Prelude hiding (Word)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.List (sortBy)
import Data.Bits (shiftL, (.|.), zeroBits, setBit, clearBit)
import Data.Array

import Fancon.Instruction.Internal
import Fancon.Memory

emit :: Array Int Instruction -> B.ByteString
emit = B.toLazyByteString . mconcat . fmap emitInstruction . elems

emitInstruction :: Instruction -> B.Builder
emitInstruction ins@Instruction{operands} =
  emitFirstByte ins <> emitOperands (sortBy packingOrder operands)

emitFirstByte :: Instruction -> B.Builder
emitFirstByte (Instruction opcode operands) = packOpcodeAndLayout opcodeByte (layoutByte operands)
  where opcodeByte = fromIntegral . fromEnum $ opcode

layoutByte :: [Operand] -> Byte
layoutByte operands = fst $ foldr f (zeroBits, (length operands - 1)) operands
  where f :: Operand -> (Byte, Int) -> (Byte, Int)
        -- TODO: Hack to omit implicit last register operand in arith
        f (Register _) (b, 1) = (b, 0)
        f (Register _) (b, ix) = (clearBit b ix, pred ix)
        f (Immediate _) (b, ix) = (setBit b ix, pred ix)

packingOrder :: Operand -> Operand -> Ordering
packingOrder (Register _) _ = LT
packingOrder _ (Register _) = GT
packingOrder (Immediate _) _ = GT

emitOperands :: [Operand] -> B.Builder
emitOperands (Register a:Register b:rest) = packNibbles a b <> emitOperands rest
emitOperands (Register a:rest) = B.word8 a <> emitOperands rest
emitOperands (Immediate a:rest) = B.word16BE a <> emitOperands rest
emitOperands [] = mempty

packNibbles :: Byte -> Byte -> B.Builder
packNibbles a b = B.word8 $ shiftL a 3 .|. b

packOpcodeAndLayout :: Byte -> Byte -> B.Builder
packOpcodeAndLayout op lay = B.word8 $ shiftL op 2 .|. lay
