module Fancon.Memory (
  RegisterFile,
  Cartridge,
  RAM,
  Address,
  Word,
  Byte,

  emptyMemory,
  emptyRAM,
  emptyCartridge,

  ram,
  readByte,
  writeByte,

  registerFile,
  storage,

  initialRegisterFile,

  hardwareStateAddress,
  inputAddress,
  interruptState,
  interruptHandlerAddress,
  ramAddress
) where

import Prelude hiding (Word)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word16, Word8)

hardwareStateAddress, inputAddress, interruptState, interruptHandlerAddress, ramAddress :: Address
hardwareStateAddress = 0
inputAddress = 1
interruptState = 2
interruptHandlerAddress = 3
ramAddress = 4

type Bank = V.Vector Byte
type Cartridge = Bank
type RAM = Bank

type Byte = Word8
type Word = Word16
type Address = Word

emptyRAM :: RAM
emptyRAM = emptyBank

emptyCartridge :: Cartridge
emptyCartridge = emptyBank

emptyBank :: Bank
emptyBank = V.replicate (fromIntegral (maxBound :: Address)) 0

emptyMemory :: Memory
emptyMemory = Memory {
  ram = emptyBank,
  registerFile = initialRegisterFile,
  storage = emptyBank,
  internalCartridge = emptyBank,
  externalCartridge = Nothing
}

initialRegisterFile :: RegisterFile
initialRegisterFile = RegisterFile {
  r0 = 0, r1 = 0, r2 = 0, r3 = 0,
  r4 = 0, r5 = 0, r6 = 0, r7 = 0,
  pc = 0, sp = 0, flags = 0
}

data Memory = Memory {
  ram :: RAM,
  registerFile :: RegisterFile,
  storage :: Bank,
  internalCartridge :: Cartridge,
  externalCartridge :: Maybe Cartridge
}

data RegisterFile = RegisterFile {
  r0 :: Word16, r1 :: Word16, r2 :: Word16, r3 :: Word16,
  r4 :: Word16, r5 :: Word16, r6 :: Word16, r7 :: Word16,
  pc :: Word16, sp :: Word16, flags :: Word16
}

readByte :: RAM -> Address -> Byte
readByte ram addr = ram V.! (fromIntegral addr)

writeByte :: RAM -> Address -> Byte -> RAM
writeByte ram addr val = ram V.// [(fromIntegral addr, val)]
