{-# Language NoStarIsType, AllowAmbiguousTypes #-}
module Fancon.Bus (
     Bus,
     Combinatorial,
     Sequential,

     bus,
     setByte, 
     setWord,
     readByte,
     readWord) where

import Data.BitVector.Sized
import GHC.TypeNats
import Data.Data
import Data.Parameterized.NatRepr

newtype Bus count = Bus (BV count)
instance KnownNat count => Show (Bus count) where
     show (Bus p) = ppBin (knownNat @count) p
     
instance Eq (Bus n) where
     (Bus l) == (Bus r) = l == r

type Combinatorial pinsCount = Bus pinsCount-> Bus pinsCount
type Sequential pinsCount = Bool -> Combinatorial pinsCount

-- | @Bus@ of width @n@ initiated to constant value
-- >>> bus @16 0xAAFF
-- 0b1010101011111111:[16]
bus :: forall (n :: Nat). KnownNat n => Integer -> Bus n
bus c = Bus $ mkBV (knownNat @n) c

-- | Sets @ix@ byte of pins to supplied value
-- >>> setByte @0 (bus 0xFF) (bus @16 0xAAAA)
-- 0b1010101011111111:[16]
setByte :: forall (ix :: Nat) (width :: Nat). (8 + 1 <= width , ix + 1 <= width `Div` 8 , KnownNat ix , KnownNat width) => Bus 8 -> Bus width -> Bus width
setByte (Bus val) (Bus dest) = Bus $ Data.BitVector.Sized.or dest shifted
    where extendedVal = zext (knownNat @width) val
          shifted = shl (knownNat @width) extendedVal $ natVal (Proxy @ix) * 8

-- | Sets @ix@ word of pins to supplied value
-- >>> setWord @0 (bus 0xFFFF) (bus @32 0xAAAAAAAA)
-- 0b10101010101010101111111111111111:[32]
setWord :: forall (ix :: Nat) (width :: Nat). (16 + 1 <= width, ix + 1 <= width `Div` 16, KnownNat ix, KnownNat width) => Bus 16 -> Bus width -> Bus width
setWord (Bus val) (Bus dest) = Bus $ Data.BitVector.Sized.or dest shifted
    where extendedVal = zext (knownNat @width) val
          shifted = shl (knownNat @width) extendedVal $ natVal (Proxy @ix) * 16

-- | Gives @ix@ byte from supplied pins
-- >>> readByte @1 (bus @24 0xAAFFAA)
-- 0b11111111:[8]
readByte :: forall (ix :: Nat) (width :: Nat). ((ix * 8) + 8 <= width, KnownNat ix) => Bus width -> Bus 8
readByte (Bus val) = Bus $ select (natMultiply (knownNat @ix) (knownNat @8)) (knownNat @8) val

-- | Gives @ix@ word from supplied pins
-- >>> readWord @1 (bus @32 0xFFFFAAAA)
-- 0b1111111111111111:[16]
readWord :: forall (ix :: Nat) (width :: Nat). ((ix * 16) + 16 <= width, KnownNat ix) => Bus width -> Bus 16
readWord (Bus val) = Bus $ select (natMultiply (knownNat @ix) (knownNat @16)) (knownNat @16) val
