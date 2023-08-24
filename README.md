# camelforth-z80

This is a port of Brad Rodriguez' Z80 CamelForth to the RC2014.
It builds using z88dk, using a Makefile.

This version compiles to a 8K ROM image, which can be used as a boot ROM:

    RC2014 - CamelForth BootROM - v20230818
    Ported to RC2014 ROM by Justin Skists
    Z80 CamelForth v1.02  25 Jan 1995

You're in! CamelForth is a 16-bit Forth, closely following the ANSI standard.

You can use ``WORDS`` to see what's available in the system.

The two CamelForth files ``glosshi.txt`` and ``glosslo.txt``contain a full list of the available words.

A good place to start is typing the following, to create your own word called `RC2014`:


    : RC2014 CR 10 0 DO ." RC2014 is cool!!" CR LOOP ;


You can then type ``RC2014`` to execute the word, and it'll print the message ten times:

    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!
    RC2014 is cool!!

    OK

Note: CamelForth is case-sensitive.

If you're new to Forth, then a good introductory is the "Starting Forth" book by Lee Brodie: https://www.forth.com/starting-forth/

# Memory Map

| Address   | Description              |
|-----------|--------------------------|
| 0000-1FFF | CamelForth ROM           |
| 2000-7FFF | Empty on RC2014 Mini     |
| 8000-87FF | CamelForth ROM Workspace |
| 8800-FFFF | USER RAM                 |

User Word definitions start at 0x8800.

The Workspace is used by CamelForth, and should be used with caution.

| Address   | Description                    |
|-----------|--------------------------------|
| 8000-81FF | ACIA driver buffers and data   |
| 8200-83FF | CamelForth workspace           |
| 8400-84FF | Terminal Input Buffer (TIB)    |
| 8500-857F | User area (128 bytes)     [IY] |
| 8580-85FF | HOLD/PAD area                  |
| 8600-86FF | Param stack               [SP] |
| 8700-87FF | Return stack              [IX] |

Note: The PAD area is version is 88 bytes long.

# Examples

Source files can be uploaded to CamelForth by using "Paste file" or "Send file" options in your terminal client.

## rc2014.fth

This is a simple Forth program that print `RC2014` in random positions on the terminal in a multitude of colours.

This file contains a number of Forth words that are used to control the terminal, (such as CLS, AT, INK, PAPER) and a
simple RANDOM number generator.

Use `RUN80` to run the demo if you're using the RC2014 Pico VGA module in 80 column, or a terminal emulator. (The Pico
VGA module with ignore the colour commands.)

Use `RUN40` to run the demo if you're using the RC2014 Pico VGA module in 40 column colour.

## blink.fth

This is a simple Forth program that will blink the LED on the RC2014.

It requires the RC2014 Digital I/O module at Port 0x00.

## hexload_test.fth

This file a proof of concept where you can embed a HEX file within the source file to send to CamelForth. The hex is the
binary of the `hello.c` Z88DK C program compiled to run at 0xA000 and the Forth code, afterwards, creates a RUN word to
call it.

This concept would allow some machine-code routines/drivers to be loaded at known addresses and Forth words written to
interface with them.

Note: Currently, HEXLOAD command must be the first line of the source file. 

# HEXLOAD

HEXLOAD is a direct port of the HEXLOAD program used for the RC2014 BASIC ROM.

To run your assembled programs using HEXLOAD, it'll go something along the lines of

    HEXLOAD
    .... upload through console like you do in BASIC. For example, code assembled for 0x9000 ...
    HEX 9000 CALL

``HEXLOAD`` initiates the HEXLOAD routine, which will wait for an Intel HEX file to be uploaded through the console.

``HEX`` will tell Forth to use the Hexadecimal number base.

``9000 CALL`` will call the program at 0x9000.

However, compiling your program to 0x9000 will leave 2048 bytes for your Forth own words.

Currently, there is no implemented method to pass parameters to machine code routines. It is recommended that you use
known locations for buffers and data passing where your Forth program writes to specified addresses, and the machine
code routine then uses them.

## hello.c

This is a simple example of how to load an Intel HEX file into CamelForth. By default, it will compile to 0x9000.

# TODO

- More examples
- incorporate (hand-compile) the words in rc2014.fth into the ROM, for convenience.
- new RSTxx hooks to allow machine code routines to PUSH and POP the Forth parameter stack (Similar to how Jupiter ACE
  works).
- Protect the Forth parameter stack a bit better, when using CALL, by setting the SP register to the end of memory before calling.
- change HEXLOAD to use the CALL word.
- fix annoying embedded HEXLOAD-in-a-file-not-on-first-line bug.

# Reference

CamelForth is from Brad Rodriguez and the ``readme.z80`` file is the original documentation.

Original Z88DK port is from James Bowman: https://github.com/jamesbowman/camelforth-z80

RC2014 BIOS code from Phillip Steven: https://github.com/feilipu/

BASIC HexLoader and notes of usage: https://github.com/RC2014Z80/RC2014/blob/master/BASIC-Programs/hexload/hexload.asm