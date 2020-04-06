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
* [Questions](#Questions--TODO)

# Hardware

## CPU

### Registers
* 16 bits wide
* R0..R7 - general purpose, RW
* R0 always reads 0, no effect on write
* PC - program counter, RO
* SP - stack pointer, RW

### Binary instruction format

First instruction byte: `ooooottt`

`o` - opcode bit

`t` - operand type bit, if set then operand is immediate, otherwise - register or unused

Operand values are packed in folowing bytes depending on `ttt` bits in first instruction byte:

`ttt` pattern       | following bytes
------------------- | ---------------
`000`               | `xxaaabbb xxxxxccc`
`001`, `010`, `100` | `xxaaabbb cccccccc cccccccc`
`101`, `110`, `011` | `xxxxxaaa bbbbbbbb bbbbbbbb cccccccc cccccccc`
`111`               | `aaaaaaaa aaaaaaaa bbbbbbbb bbbbbbbb cccccccc cccccccc`

Where `x` - unused bit, `a`, `b`, `c` - instruction arguments

## Memory map

Addr range  | Size  | Device
----------- | ----- | --------------------
00000-00006 | 6     | [Interrupt controller](#Interrupt-controller)
00006-00014 | 8     | [Cartridge controller](#Cartridge-controller)
00014-16455 | 16441 | [GPU](GPU)
16455-16456 | 1     | [Input controller](#Input-controller)
16456-17480 | 512   | ROM
17480-23551 | 6071  | Unused
23551-24575 | 1024  | [Battery-backed](#RAM)
24575-65535 | 40960 | [RAM](#RAM)

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
00001  | RW   | Busy bit
00002  | RW   | SRC word
00004  | RW   | DST word
00006  | RW   | LEN word

Present byte:
* 1 when cartridge present
* 0 otherwise

Busy byte:
* 1 when data transfer in progress
* 0 when no data transfer in progress
* Write 1 when 0 to start data transfer
* Resets to 0 when data transfer completed

SRC word:
* Starting address in cartridge mem to read from during data transfer

DST word:
* Starting address in main memory to write to during data transfer

LEN word:
* Length of buffer to transfer

### GPU

Offset | Mode | Description
------ | ---- | -----------
00000  | RW   | Palette
00048  | RW   | Sprite map
16432  | RW   | Control
16433  | RW   | Command argument register 1
16435  | RW   | Command argument register 2
16437  | RW   | Command argument register 3
16439  | RW   | Command argument register 4

Palette 48 bytes:
* Array of 16 colors
* Each color is 3 RGB bytes

Sprites 16384 bytes:
* Array of 512 sprites
* Sprite is 8x8 pixel grid
* Pixel is 4 bit index to pallete

Control byte:
* `(IIII CCCC)`
* `(B)` Busy bit
  * 1 when GPU command is being processed
  * 0 when GPU is ready for next command
  * Write 1 when 0 to start executing command
  * Resets to 0 when command execution finished
* `(C)` Command 4 bit index
  * Index of GPU command to execute
  * 0x0: sprite
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
Sprite   | 0     | SprIx, X, Y    | Blit sprite with SprIx to X,Y position, where X,Y is top-left corner
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

Offset | Mode | Description
------ | ---- | -----------
00000  | RW   | Battery-backed RAM
01024  | RW   | Conventional RAM

Battery-backed RAM, 1kb:
* Acts as conventional RAM but persists its data

Conventional RAM, 40960 bytes

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
add      | r/i | r/i | r   |
sub      | r/i | r/i | r   |
div      | r/i | r/i | r   |
mul      | r/i | r/i | r   |
xor      | r/i | r/i | r   |
shf      | r/i | r/i | r   |
and      | r/i | r/i | r   |
or       | r/i | r/i | r   |
save     | r/i | r/i |     | Write A to mem[B]
saveh    | r/i | r/i |     | Write A to mem[0xFFFF + B]
load     | r/i | r   |     | Write mem[A] to register B
loadh    | r/i | r   |     | Write mem[0xFFFF + A] to register B
jgz      | r   | r/i |     | Jump to mem[B] if A is >0
jlt      | r   | r/i |     | Jump to mem[B] if A is <0
jez      | r   | r/i |     | Jump to mem[B] if A is 0
int      |     |     |     | Interrupt
brk      |     |     |     | Break

# Questions / TODO

## Gamepad layout

```
 ^    XYZ
< > S ABC
 v
```

Keymap:
XYZ - ASD
ABC - ZXC
S   - Enter

## Word-addressable memory
Instead of addressing bytes, address 16-bit words
Pros: 128k addressable
Cons: Bit-fiddling to get a byte, gotta rework all memmap, probably API too, all instructions become 2 words with lots of unused bits

## Halt instruction
Stops execution until interrupt, noop if all interrupts masked

## Add `data` and/or `var` commands to assembler
`.var var-name var-size` - Reserve `var-size` somewhere in binary and then resolve `var-name` references with its address
`.data data-name data-value` - Put `data-value` somewhere in binary and then resolve `data-name` references with its address

## Replace `import`/`export` commands with single `global`?
Pros: less cases to handle in symtab code
Cons: less flexible?

## Developer Unit
### Code editor
bytes left count, follow jump, outline?, basic vim bindings
### Sprite editor
Copy TIC80, add font mode
### Console
Help command
Code editor
Sprite editor
Cartridge API

### Debug mode
#### Projects
Cartridge name + save bookmarks
#### Code view
Show opcodes, registers, stack memory, PC pointer
#### Memory view
* Bookmarks list
  * Custom: addr + view mode
  * VRAM
  * RAM
* Raw
* Opcodes
* Font
* Sprite (size multiplyer)

## Cartridges

Some template png
Embed game cartridge binary into template
```
   /--------------\ <- sloped
   |xx|xx|xx|xx|xx|
   |xx|xx|xx|xx|xx| <- contact pads, embed code on them
   |xx|xx|xx|xx|xx|
   |              |
   | __Game______ | <- sticker like area for text
   | ___name_____ |
   | ____v0.1____ |
   +------------- +
```

## 16 registers
2 bits in instruction byte for register indexes are unused.
Make indexes 4 bit and allow to address 16 registers
