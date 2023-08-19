# camelforth-z80

This is a port of Brad Rodriguez' Z80 CamelForth to the RC2014.
It builds using z88dk, using a Makefile.

This version compiles to a 8K ROM image, which can be used as a boot ROM:

    RC2014 - CamelForth BootROM - v20230818
    Ported to RC2014 ROM by Justin Skists
    Z80 CamelForth v1.02  25 Jan 1995

You're in! CamelForth is a 16-bit Forth, closely following the ANSI standard.

You can use ``WORDS`` to see what's available in the system.

The two CamelForth files
``glosshi.txt`` and ``glosslo.txt``
contain a full list of the available words.


    : RC2014 10 0 DO ." RC2014 Mini is cool!!" CR LOOP ;

Note: CamelForth is case sensitive.

# Memory Map

| Address   | Description              |
|-----------|--------------------------|
| 0000-1FFF | CamelForth ROM           |
| 2000-7FFF | Empty on RC2014 Mini     |
| 8000-8FFF | CamelForth ROM Workspace |
| 9000-FFFF | USER RAM                 |

User Word definitions start at 0x9000.

The Workspace is used by CamelForth for temporary storage, and is not available to the user.

| Address   | Description                    |
|-----------|--------------------------------|
| 8000-81FF | ACIA driver buffers and data   |
| 8200-83FF | CamelForth workspace           |
| 8400-84FF | Terminal Input Buffer (TIB)    |
| 8500-857F | User area (128 bytes)     [IY] |
| 8580-85FF | HOLD/PAD area                  |
| 8600-86FF | Param stack               [SP] |
| 8700-87FF | Return stack              [IX] |



# Examples

## rc2014.fth

This is a simple Forth program that will tell you what CamelForth thinks your RC2014 is.

## blink.fth

This is a simple Forth program that will blink the LED on the RC2014.

It requires the RC2014 Digital I/O module at Port 0x00.

# HEXLOAD

HEXLOAD is a direct port of the HEXLOAD program used for the RC2014 BASIC ROM.

To run your assembled programs using HEXLOAD, it'll go something along the lines of

    HEXLOAD
    .... upload through console like you do in BASIC. For example, code assembled for 0x9000 ...
    HEX 9000 CALL

``HEXLOAD`` initiates the HEXLOAD routine, which will wait for an Intel HEX file to be uploaded through the console.

``HEX`` will tell Forth to use the Hexadecimal number base.

``9000 CALL`` will call the program at 0x9000.

However, compiling your program to 0x9000 won't leave much space for your Forth words.

## hello.c

This is a simple example of how to load an Intel HEX file into CamelForth.



# Reference

CamelForth is from Brad Rodriguez and the ``readme.z80`` file is the original documentation.

Original Z88DK port is from James Bowman: https://github.com/jamesbowman/camelforth-z80

RC2014 BIOS code from Phillip Steven: https://github.com/feilipu/

BASIC HexLoader and notes of usage: https://github.com/RC2014Z80/RC2014/blob/master/BASIC-Programs/hexload/hexload.asm