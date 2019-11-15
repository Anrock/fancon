# Fantasy console

# Table of contents
* [Memory map](#Memory-map)
* [Registers](#registers)
* [API](#API)
  * [Call convention](#Call-convention)
  * [Cartridge](#Cartridge)
  * [Graphics](#Graphics)
  * [Storage](#Storage)
  * [Output](#Output)
* [Cartridges](#Cartridges)
  * [System cartridge](#System-cartridge)
    * [Code editor](#Code-editor)
    * [Sprite editor](#Sprite-editor)
    * [Console](#Console)
* [Questions](#Questions)

# Memory map
```
+-------------------------------------+--------------+-----+
| Description                         | Size / Bits  | Mod |
+-------------------------------------+--------------+-----+
= HARDWARE STATE ===========================================
| Display, Input, Cartridge           | xxxx xDIC    | r-- |
= INPUT ====================================================
| Pressed, Char                       | P CCCCCCC    | r-- |
= INTERRUPT ================================================
| Breakpoint, Cartridge, Input, Timer | BCIT bcit    | rw- |
| Handler addr                        | 1            | -w- |
= RAM ======================================================
| VRAM map                            | 16432 / 8944 |     |
+-------------------------------------+--------------+-----+

+-------------------+-------+-----+
| Graphic mode VRAM | Size  | Mod |
+-------------------+-------+-----+
| Pallete           | 48    | rw- | 16 RGB colors
| Sprite map        | 8192  | rw- | 256 8x8 sprites, each pixel is color index
| Sprite map        | 8192  | rw- | 256 8x8 sprites, each pixel is color index
+-------------------+-------+-----+
| Total             | 16432       |
+-------------------+-------+-----+

+----------------+------+-----+
| Text mode VRAM | Size | Mod |
+----------------+------+-----+
| Palette        | 48   | rw- | 16 RGB colors
| Font map       | 4096 | rw- | 256 symbols * 8*16 dots
| Text buffer    | 4800 | rw- | char byte + fg color + bg color
+----------------+------+-----+
| Total          | 8944       |
+----------------+------+-----+

+---------------+------+-----+
| Cartridge     | Size | Mod |
+---------------+------+-----+
| Palette       | 48   | rw- | Mapped to VRAM palette
| Graphics data |      | rw- |
| code          |      | r-x | Mapped to RAM
+---------------+------+-----+
```

# Registers
r0..r7, flags, pc

r0 is used as sp

Flags: 0, <0, >0

# API
## Interrupts
Program can alter flags in interrupt mask to mask or unmask interupts

When interrupt happens CPU will:
1. Set corresponding flag in interrupt state
2. Push PC to stack
3. Push registers to stack in reverse order (r8..r1)
4. Jump to interrupt handler address

Interrupt handler subroutine should:
1. (Optional) Check interrupt state to determine a type of interrupt
2. (Optional) Handle the interrupt
3. Unset interrupt bits in interrupt state
4. Restore registers from stack (r1..r8)
5. Jump to PC saved on stack after registers

## System calls
To make a sys call
1. Put interrupt number to `r1`
2. Put args in (`r2`..`r7`) in direct order
3. Execute int opcode
4. Read result from `r1`

```
+----------+----+-----+-----+-----+----+----+-----+
| mnemonic | r1 | r2  | r3  | r4  | r5 | r6 | r7  |
+----------+----+-----+-----+-----+----+----+-----+
= Cartridge =======================================
| cart     | 0  |     |     |     |    |    |     |
| burn     | 1  | src | dst | len |    |    |     |
= Graphics ========================================
| sprite   | 2  | idx | x   | y   |    |    |     |
| line     | 3  | idx | x1  | y1  | x2 | x1 | y2  |
| fill     | 4  | idx | x1  | y1  | x2 | x1 | y2  |
| scroll   | 5  | idx | x   | y   |    |    |     |
| mode     | 6  |     |     |     |    |    |     |
= Storage =========================================
| save     | 7  | len | src | dst |    |    |     |
| load     | 8  | len | src | dst |    |    |     |
| peek     | 9  | src |     |     |    |    |     |
| bank     | 10 | idx |     |     |    |    |     |
= Debug ===========================================
| out      | 11 | len | src |     |    |    |     |
+----------+----+-----+-----+-----+----+----+-----+
```

### Cartridge
**cart**

Map external cartridge memory to ram

Does nothing if external cartridge is not inserted

**burn: src, dst, len**

Burn `len` bytes from `RAM[<src>]` to `dst` of external cartridge

Will issue a memory fault if any byte in range `[src, src + len]` isn't readable

### Graphics
**sprite: index (0..511), x (0..319), y (0..239)**

Blit sprite with `index` to `(x,y)` pixel. Where `(x,y)` is top-left corner

Will issue a gfx fault if any of arguments have invalid value

**line: x1 (0..319), y1 (0..239), x2 (0..319), y2 (0..239), color (0..15)**

Draw a `pallete[color]` line from `(x1,y1)` to `(x2,y2)`

Will issue a gfx fault if any of arguments have invalid values

**fill: x1 (0..319), y1 (0..239), x2 (0..319), y2 (0..239), color (0..15)**

Draw a rectangle filled with `palette[color]`
with top level corner `(x1,y1)` and right bottom corner at `(x2,y2)`

Will issue a gfx fault if any of arguments have invalid values

**scroll: color, x (signed), y (signed)**

Scroll screen to horizontally by `x` and vertically by `y`, filling new space with `color`

**mode**

Toggle video mode from text to graphics or vice versa.

### Storage
**save: src (>=RAM), storage, len**

Write `len` bytes from `RAM[src]` to `storage`

Will issue a memory fault if any byte in range `[src, src + len]` isn't readable

**load: src, dst, len**

Write `len` bytes from `storage[src]` to `RAM[dst]`

Will issue a memory fault if any byte in range `[dst, dst + len]` isn't writable

**peek: src**

Write `storage[src]` to `r1`

**bank**: idx

Switch storage bank to `idx`

### Output
**out: src, len**

Output `len` bytes starting with `src`

Will issue a memory fault if any byte in range `[src, src + len]` isn't readable

# Cartridges
## System cartridge
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

## Cartridge creation
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

# Assembly
## Syntax
`# ignored` - beginning of comment, everything after it till eol is ignored
`<instruction>`
`<label>:` - defines a label, alias `<label>` to address of next instruction
`external <symbol>` - treat `<symbol>` as external
`export <symbol>` - make `<symbol>` available in other files via `external`

```
add r r/i r
sub r r/i r
div r r/i r
mul r r/i r

xor r r/i r
shl r r/i r
shr r r/i r
and r r/i r
or  r r/i r

savb r r/i
lodb r/i r
savw r r/i
lodw r/i r

jmp r/i
jgz r/i
jlt r/i
jez r/i

int
brk
```

## Binary instruction format
`x` - unused bit
`S` - source register bit
`D` - destination register bit
`A` - argument register bit
`I` - immediate value bit
```
1OOOOOOO SSSSDDDD xxxxAAAA
0OOOOOOO SSSSDDDD IIIIIIII IIIIIIII
```

# Questions
## Check for something useful in Universal.md
TODO

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

## Memory mapped storage
Pros: flat memory map
Cons: take 4 bytes of memory map + 1 new interrupt

