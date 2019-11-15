module Fancon.Memory (
  Memory,
  RegisterFile,
  ram,
  registerFile,
  storage,
  initialMemory,
  initialRegisterFile,
  hardwareStateAddress,
  inputAddress,
  interruptState,
  interruptHandlerAddress,
  ramAddress
) where

import Data.Array
import Data.Word (Word16)

hardwareStateAddress, inputAddress, interruptState, interruptHandlerAddress, ramAddress :: Word16
hardwareStateAddress = 0
inputAddress = 1
interruptState = 2
interruptHandlerAddress = 3
ramAddress = 4

type Bank = Array Word16 Word16

emptyBank :: Bank
emptyBank = array (0, maxBound :: Word16) [(i, 0) | i <- [0..maxBound :: Word16]]

initialMemory :: Memory
initialMemory = Memory {
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
  pc = 0, flags = 0
}

data Memory = Memory {
  ram :: Bank,
  registerFile :: RegisterFile,
  storage :: Bank,
  internalCartridge :: Bank,
  externalCartridge :: Maybe Bank
}

data RegisterFile = RegisterFile {
  r0 :: Word16, r1 :: Word16, r2 :: Word16, r3 :: Word16,
  r4 :: Word16, r5 :: Word16, r6 :: Word16, r7 :: Word16,
  pc :: Word16, flags :: Word16
}
