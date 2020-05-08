# Fantasy console

# Table of contents
* [Hardware](#Hardware)
  * [CPU](#CPU)
    * [Registers](#Registers)
    * [Binary instruction format](#Binary-instruction-format)
  * [Memory map](#Memory-map)
  * [Devices](#Devices)
    * [Interrupt controller](#Interrupt-controller)
    * [Cartridge controller](#Cartridge-controller)
    * [GPU](#GPU)
      * [GPU commands reference](#GPU-commands-reference)
    * [Input controller](#Input-controller)
    * [RAM](#RAM)
* [Software](#Software)
  * [Assembly](#Assembly)

# Hardware

## CPU

### Registers
* 16 bits wide
* R0..R7 - general purpose, RW
* R0 always reads 0, no effect on write
* PC - program counter, RO
* SP - stack pointer, RW

### Binary instruction format

Each instruction consists of 1 up to 5 bytes.

First byte is instruction description byte, rest is instruction operands

Description byte format is `TT?????` where `TT` is instruction type and rest of
bits are type-specific.

Instruction type may be of `11`, `01`, `10` and `00` where
* `11` is A-instruction
* `01` is M-instruction
* `10` is J-Instruction
* `00` is S-instruction

A-instruction uses three operands: A, B and C.
Value of register A is combined with B using operation specified by F and then 
stored to register specified by C. A-instruction-specific bit format is as follows:
`FFFxxI`, where `F` specifies operation:
* `000` for addition
* `001` for substraction
* `010` for division
* `011` for multiplication
* `100` for bitwise xor
* `101` for bit shift
* `110` for bitwise and
* `111` for bitwise or
`I`mmediate indicates if B is an immediate value

M-instructions are using two operands and have this specific bit pattern:
`FHWxAB`. `F` is operation - `1` for store and `0` for load. `H`igh indicates if
0xFFFF should be added to memory address specified by operand in order to access
addresses above 0xFFFF. `W`ord indicates if stored\loaded value is word or a
byte. First operand specifies memory address and second operand is either a
value to write in case of store or a destination register for load. `AB` bits
are set accordingly. Load can only have register as destination, so `A` is
unused if `F` is `0`.

J-instruction are using two operands. First operand is a register to check for
condition and second operand is immediate or register containing address for a
jump. Specific bits: `CCxxxI`, where `C`ondition may be one of
* `00` for greater than zero
* `01` for less than zero
* `10` for equal to zero
`I` indicates that second operand is immediate.

S-instruction don't have operands. Only first bit is significant, indicating if
it's `int` for `1` or `brk` if it's `0`.

## Memory map

Addr range  | Size  | Device
----------- | ----- | --------------------
00000-00006 | 6     | [Interrupt controller](#Interrupt-controller)
00006-00007 | 1     | [Input controller](#Input-controller)
00007-00014 | 7     | [Cartridge controller](#Cartridge-controller)
00014-16456 | 16441 | [GPU](GPU)
16456-16968 | 512   | ROM
16968-17406 | 438   | Reserved
17406-18430 | 1024  | [Battery-backed](#RAM)
18430-65535 | 47105 | [RAM](#RAM)

## Devices

### Interrupt controller

Offset | Mode | Description
------ | ---- | ----------------------
00000  | RO   | Return address
00002  | RW   | Interrupt mask
00003  | RW   | Interrupt state
00004  | RW   | Interrupt handler addr

Return address word:
* Stores PC when interrupt happened before jumping to handler

Interrupt mask byte:
* When bit is 1 - interrupt is masked
* When interrupt masked handler won't be invoked when interrupt raised
* All interrupts are masked before interrupt handler is invoked
* 0 bit: Data transfer finished
* 1 bit: Cart presence changed
* 2 bit: GPU command finish
* 3 bit: GPU invalid command
* 4 bit: Input state change
* 5 bit: Clock
* 6 bit: Reserved
* 7 bit: Reserved

Interrupt flags byte:
* Indexes are same as in mask byte
* Bit set to 1 when interrupt is active
* Set corresponding bit to 0 to mark interrupt as handled

Interrupt handler address word
* Stores address to jmp to when interrupt is raised

### Cartridge controller

Offset | Mode | Description
------ | ---- | -----------
00000  | RO   | State bit
00001  | RW   | SRC word
00003  | RW   | DST word
00005  | RW   | LEN word

Present byte:
* 1 when cartridge present
* 0 otherwise

SRC word:
* Starting address in cartridge mem to read from during data transfer

DST word:
* Starting address in main memory to write to during data transfer

LEN word:
* Length of buffer to transfer
* Non-zero when data transfer in progress
* Write non-zero value to start data transfer
* Resets to 0 when data transfer completed

### GPU

Offset | Mode | Description
------ | ---- | -----------
00000  | RW   | Command argument register 1
00002  | RW   | Command argument register 2
00004  | RW   | Command argument register 3
00006  | RW   | Command argument register 4
00008  | RW   | Control
00009  | RW   | Transparency mask
00010  | RW   | Palette
00058  | RW   | Sprite map

Palette 48 bytes:
* Array of 16 colors
* Each color is 3 RGB bytes

Sprites 16384 bytes:
* Array of 512 sprites
* Sprite is 8x8 pixel grid
* Pixel is 4 bit index to pallete

Control byte:
* `(IIII CCCC)`
* `(C)` Command 4 bit index
  * Index of GPU command to execute
  * 0x0: blit
  * 0x1: line
  * 0x2: fill
  * 0x3: scroll
  * 0x4: pixel
  * Write command index to start executing command
  * Resets to 0 when command execution finished
* `(I)` Color 4 bits:
  * Index of color from palette to use when drawing

#### GPU commands reference

Argument | Valid values
---------| ------------
SprIx    | 0..511
X        | 0..319
Y        | 0..239
N, M     | 0..65535
Color    | 0..15

Mnemonic | Index | Arguments      | Description
-------- | ----- | -------------- | -----------
Blit     | 0     | SprIx, X, Y    | Blit sprite with SprIx to X,Y position, where X,Y is top-left corner
Line     | 1     | X1, Y1, X2, Y2 | Draw a line from X1,Y1 to X2,Y2 using current color
Fill     | 2     | X1, Y1, X2, Y2 | Draw filled rectangle with current color with top level corner X1,Y1 and right bottom corner at X2,Y2
Scroll   | 3     | N, M           | Scroll screen to horizontally by N and vertically by M pixels, filling new space with current color
Pixel    | 4     | X, Y           | Draw a pixel at X,Y using current color

### Input controller

Offset | Mode | Description
------ | ---- | -----------
00000  | RO   | Input state

Input state byte:
* `(PCCCCCCC)`
* `(P)` - button pressed state
* `(C)` - character of pressed button

### RAM

Battery-backed: persists its contents without power
RAM: conventional RAM, 16bits width, byte-addressable

# Software

## Assembly

###  Syntax
`<instruction>`
`.<command>`

Commands:
  * label <label> - mark next instruction address as <label>
  * export <label> - make <label> available to use in another files
  * import <label> - use <label> address from another file
  * const <name> <val> - define constant value of <val> named <name>
  * <empty> - ignore everything until eol, used as comment

### Instructions

Mnemonic | A   | B   | C   | Note
-------- | --- | --- | --- | ----
add      | r   | r/i | r   |
sub      | r   | r/i | r   |
div      | r   | r/i | r   |
mul      | r   | r/i | r   |
xor      | r   | r/i | r   |
shf      | r   | r/i | r   |
and      | r   | r/i | r   |
or       | r   | r/i | r   |
save     | r/i | r/i |     | Write lower byte of A to mem[B]
savew    | r/i | r/i |     | Write A to mem[B]
saveh    | r/i | r/i |     | Write lower byte of A to mem[0xFFFF + B]
savehw   | r/i | r/i |     | Write A to mem[0xFFFF + B]
load     | r/i | r   |     | Write mem[A] to lower byte of register B
loadw    | r/i | r   |     | Write word at mem[A] to register B
loadh    | r/i | r   |     | Write mem[0xFFFF + A] to lower byte of to register B
loadhw   | r/i | r   |     | Write word at mem[0xFFFF + A] to register B
jgz      | r   | r/i |     | Jump to mem[B] if A is >0
jlt      | r   | r/i |     | Jump to mem[B] if A is <0
jez      | r   | r/i |     | Jump to mem[B] if A is 0
int      |     |     |     | Interrupt
brk      |     |     |     | Break

Instructions not implemented but can be improvised from existings ones:

Mnemonic | A   | B   | C   | Instead use
-------- | --- | --- | --- | -----------
set      | r/i | r   |     | add r0 A B
nop      |     |     |     | add r0 r0 r0 (or any other arith or logic instruction)
cmp      | r/i | r/i | r   | sub A B C
inc      | r   |     |     | add A 1 A
dec      | r   |     |     | sub A 1 A
jmp      | r/i |     |     | jez r0 A
