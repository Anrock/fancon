module Fancon.Memory
  ( Address
  , bytesToWord
  , wordToBytes
  , byteToNibbles
  ) where

import Prelude hiding (Word)
import Data.Word (Word16, Word8)
import Data.Bits (shiftL, shiftR, (.&.))

type Address = Word16

bytesToWord :: Word8 -> Word8 -> Word16
bytesToWord hi lo = fromIntegral $ shiftL (0xFF .&. hi) 8 + lo

wordToBytes :: Word16 -> (Word8, Word8)
wordToBytes w = (fromIntegral . shiftR (0xF0 .&. w) $ 8, fromIntegral $ 0x0F .&. w)

byteToNibbles :: Word8 -> (Word8, Word8)
byteToNibbles b = (shiftR (0b11110000 .&. b) 4, 0b00001111 .&. b)
