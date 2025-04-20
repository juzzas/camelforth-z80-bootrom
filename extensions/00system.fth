CamelForth BootROM for RC2014 - System Disk

This system disk contains extensions, tools, and demos that can
be loaded into the forth environment.

Screen 1 contains some constants that allow ease of loading
extensions to the kernel:
   1 LOAD

Screen 2 contains a script to load a system that you can then
save to disk (for example; block 20):
   2 LOAD   20 SAVE

This can be reloaded using:
   20 RESTORE

\ CamelForth BootROM system disk

30 CONSTANT TOOLING
60 CONSTANT EDITING
90 CONSTANT ASSEMBLING
120 CONSTANT TASKING
150 CONSTANT BLKFILING
210 CONSTANT Z80ASSEMBLING
300 CONSTANT FORTH2012







\ CamelForth System generator
1 LOAD
TOOLING LOAD
EDITING LOAD
TASKING LOAD
BLKFILING LOAD
180 LOAD
\ ASSEMBLING LOAD
Z80ASSEMBLING LOAD
FORTH2012 LOAD

\ 20 SAVE

