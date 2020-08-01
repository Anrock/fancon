module Fancon.CPU where

import Data.Word
import qualified Data.Vector as V

data Clock = High | Low deriving (Eq, Show)

data CPUPins = CPUPins {
  clk :: Clock,
  intr :: Bool,

  memoryData :: Word16,
  memoryAddress :: Word16,
  memoryByte :: Bool,
  writeEnable :: Bool
} deriving (Eq, Show)

data CPU = CPU {
  pins :: CPUPins,
  registers :: V.Vector Word16,
  pc :: Word16
} deriving (Eq, Show)
