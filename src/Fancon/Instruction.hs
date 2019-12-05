module Fancon.Instruction
  ( Opcode(..)
  , Operands(..)
  , Instruction, opcode, operands
  , r0, r1, r2, r3, r4, r5, r6, r7
  , add, sub, div, mul, xor, shf, and, or
  , addi, subi, divi, muli, xori, shfi, andi, ori
  , save, savei, load, loadi
  , jmp, jmpi, jgz, jgzi, jlt, jlti, jez, jezi
  , int, brk
  ) where

import Prelude hiding (div, and, or, Word)

import Fancon.Memory

data Opcode = AddR  | AddI
            | SubR  | SubI
            | DivR  | DivI
            | MulR  | MulI
            | XorR  | XorI
            | ShfR  | ShfI
            | AndR  | AndI
            | OrR   | OrI
            | SaveR | SaveI
            | LoadR | LoadI
            | JmpR  | JmpI
            | JgzR  | JgzI
            | JltR  | JltI
            | JezR  | JezI
            | Int
            | Brk
            deriving (Eq, Show, Bounded, Enum)

data Operands = RRR Byte Byte Byte
              | RR Byte Byte
              | R Byte
              | I Word
              | RRI Byte Byte Word
              | RI Byte Word
              | None
              deriving (Eq, Show)

data Instruction = Instruction { opcode :: Opcode
                               , operands :: Operands
                               } deriving (Eq, Show)

r0, r1, r2, r3, r4, r5, r6, r7 :: Byte
r0 = 0
r1 = 1
r2 = 2
r3 = 3
r4 = 4
r5 = 5
r6 = 6
r7 = 7

rrr :: Opcode -> Byte -> Byte -> Byte -> Instruction
rrr op a b c = Instruction op (RRR a b c)

rri :: Opcode -> Byte -> Word -> Byte -> Instruction
rri op a b c = Instruction op (RRI a c b)

rr :: Opcode -> Byte -> Byte -> Instruction
rr op a b = Instruction op (RR a b)

ri :: Opcode -> Byte -> Word -> Instruction
ri op a b = Instruction op (RI a b)

none :: Opcode -> Instruction
none op = Instruction op None

add, sub, div, mul, xor, shf, and, or :: Byte -> Byte -> Byte -> Instruction
add = rrr AddR
sub = rrr SubR
div = rrr DivR
mul = rrr MulR
xor = rrr XorR
shf = rrr ShfR
and = rrr AndR
or  = rrr OrR

addi, subi, divi, muli, xori, shfi, andi, ori :: Byte -> Word -> Byte -> Instruction
addi = rri AddI
subi = rri SubI
divi = rri DivI
muli = rri MulI
xori = rri XorI
shfi = rri ShfI
andi = rri AndI
ori  = rri OrI

save, load :: Byte -> Byte -> Instruction
save = rr SaveR
load = rr LoadR

savei :: Byte -> Word -> Instruction
savei = ri SaveI

loadi :: Word -> Byte -> Instruction
loadi a b = ri LoadI b a

jmp :: Byte -> Instruction
jmp a = Instruction JmpR (R a)

jmpi :: Word -> Instruction
jmpi a = Instruction JmpI (I a)

jgz, jlt, jez :: Byte -> Byte -> Instruction
jgz = rr JgzR
jlt = rr JltR
jez = rr JezR

jgzi, jlti, jezi :: Byte -> Word -> Instruction
jgzi = ri JgzI
jlti = ri JltI
jezi = ri JezI

int, brk :: Instruction
int = none Int
brk = none Brk
