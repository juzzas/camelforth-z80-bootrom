INCLUDE tools/blocks.fth

600 GENERATE: build/cf-image.blk
OPEN:  build/cf-image.blk
0 AT-BLOCK: extensions/00system.fth
30 AT-BLOCK: extensions/forth2012.fth
33 AT-BLOCK: extensions/core-ext.fth
36 AT-BLOCK: extensions/facility-ext.fth
39 AT-BLOCK: extensions/double.fth
42 AT-BLOCK: extensions/string.fth
45 AT-BLOCK: extensions/tools-ext.fth
54 AT-BLOCK: extensions/memory.fth

90 AT-BLOCK: extensions/fixed.fth
99 AT-BLOCK: extensions/random.fth
102 AT-BLOCK: extensions/ansi.fth
108 AT-BLOCK: extensions/leds.fth

180 AT-BLOCK: extensions/assembler.fth

300 AT-BLOCK: extensions/testsuite.fth
303 AT-BLOCK: extensions/ttester.fth
330 AT-TEXT: forth2012-test-suite/src/prelimtest.fth
360 AT-TEXT: forth2012-test-suite/src/core.fr
FLUSH
BYE
