module Fancon.Emit (emit, pack) where

import Prelude hiding (Word)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B

import Data.Bits (shiftL, (.|.))

import Fancon.Instruction
import Fancon.Memory

emit :: [Instruction] -> B.ByteString
emit = B.toLazyByteString . mconcat . fmap emitInstruction

emitInstruction :: Instruction -> B.Builder
emitInstruction ins = emitOpcode op <> emitOperands ops
  where op = opcode ins
        ops = operands ins

emitOpcode :: Opcode -> B.Builder
emitOpcode = B.word8 . fromIntegral . fromEnum

emitOperands :: Operands -> B.Builder
emitOperands (RRR a b c) = pack a b <> B.word8 c
emitOperands (RRI a b c) = pack a b <> B.word16BE c
emitOperands (RR a b) = pack a b
emitOperands (R a) = B.word8 a
emitOperands (I a) = B.word16BE a
emitOperands (RI a b) = B.word8 a <> B.word16BE b
emitOperands None = mempty

pack :: Byte -> Byte -> B.Builder
pack a b = B.word8 $ shiftL a 3 .|. b
