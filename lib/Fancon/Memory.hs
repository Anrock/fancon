module Fancon.Memory
  ( Address
  , bytesToWord
  , wordToBytes
  , firstByte
  , secondByte
  , byteToNibbles
  , Word8
  , Word16
  , (.>.)
  , (.<.)
  ) where

import Prelude hiding (Word)
import Data.Word (Word16, Word8)
import Data.Bits (shiftL, shiftR, (.&.), Bits)

type Address = Word16

bytesToWord :: Word8 -> Word8 -> Word16
bytesToWord hi lo = fromIntegral $ shiftL (0xFF .&. hi) 8 + lo

wordToBytes :: Word16 -> (Word8, Word8)
wordToBytes w = (fromIntegral . shiftR (0xF0 .&. w) $ 8, fromIntegral $ 0x0F .&. w)

firstByte :: Word16 -> Word8
firstByte = fst . wordToBytes

secondByte :: Word16 -> Word8
secondByte = snd . wordToBytes

byteToNibbles :: Word8 -> (Word8, Word8)
byteToNibbles b = (shiftR (0b11110000 .&. b) 4, 0b00001111 .&. b)

infixl 7 .>.
(.>.) :: Bits a => a -> Int -> a
(.>.) = shiftR

infixl 7 .<.
(.<.) :: Bits a => a -> Int -> a
(.<.) = shiftL

