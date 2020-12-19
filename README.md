# camelforth-z80

This is a port of Brad Rodriguez' Z80 CamelForth to the RC2014.
It builds using z88dk, using a Makefile.

To run it, compile it and burn to an EPROM for the RC2014 Pageable ROM.

    RC2014 - CamelForth BootROM
    z88dk - feilipu
    BootROM modifications - Justin Skists
    Z80 CamelForth v1.02  25 Jan 1995


You're in! CamelForth is a 16-bit Forth, closely following the ANSI standard.

You can use ``WORDS`` to see what's available in the system.

The two CamelForth files
``glosshi.txt`` and ``glosslo.txt``
contain a full list of the available words.

On reset, you have the option of a cold start or a warm start.

    RC2014 - CamelForth BootROM
    z88dk - feilipu
    BootROM modifications - Justin Skists

    Cold | Warm start (C|W) ? 
    
A warm start will keep the current dictionary in memory.

# Reference

Original code from James Bowman: https://github.com/jamesbowman/camelforth-z80

RC2014 BIOS code from Phillip Steven: https://github.com/feilipu/
