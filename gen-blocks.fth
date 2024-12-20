INCLUDE tools/blocks.fth

1000 GENERATE: build/cf-image.blk
OPEN:  build/cf-image.blk
0 AT-BLOCK: extensions/00system.fth
3 AT-BLOCK: extensions/editor.fth
30 AT-BLOCK: extensions/forth2012.fth
33 AT-BLOCK: extensions/core-ext.fth
36 AT-BLOCK: extensions/facility-ext.fth
39 AT-BLOCK: extensions/double.fth
42 AT-BLOCK: extensions/string.fth
45 AT-BLOCK: extensions/tools-ext.fth
54 AT-BLOCK: extensions/memory.fth

78 AT-BLOCK: extensions/multi.fth
81 AT-BLOCK: extensions/blkfile.fth
90 AT-BLOCK: extensions/fixed.fth
99 AT-BLOCK: extensions/random.fth
102 AT-BLOCK: extensions/ansi.fth
108 AT-BLOCK: extensions/leds.fth

180 AT-BLOCK: extensions/assembler.fth

300 AT-BLOCK: extensions/testsuite.fth
303 AT-BLOCK: extensions/ttester.fth
330 AT-TEXT: test/forth2012-test-suite/src/prelimtest.fth
342 AT-TEXT: test/forth2012-test-suite/src/core.fr
372 AT-TEXT: test/forth2012-test-suite/src/coreplustest.fth
384 AT-TEXT: test/forth2012-test-suite/src/utilities.fth
390 AT-TEXT: test/forth2012-test-suite/src/errorreport.fth
396 AT-TEXT: test/forth2012-test-suite/src/coreexttest.fth
423 AT-TEXT: test/forth2012-test-suite/src/blocktest.fth
450 AT-TEXT: test/forth2012-test-suite/src/doubletest.fth
465 AT-TEXT: test/forth2012-test-suite/src/exceptiontest.fth
471 AT-TEXT: test/forth2012-test-suite/src/facilitytest.fth
477 AT-TEXT: test/forth2012-test-suite/src/filetest.fth
489 AT-TEXT: test/forth2012-test-suite/src/localstest.fth
498 AT-TEXT: test/forth2012-test-suite/src/memorytest.fth
504 AT-TEXT: test/forth2012-test-suite/src/toolstest.fth
519 AT-TEXT: test/forth2012-test-suite/src/searchordertest.fth
528 AT-TEXT: test/forth2012-test-suite/src/stringtest.fth

600 AT-BLOCK: test/hexload_test.fth
612 AT-BLOCK: test/hexload_test2.fth
621 AT-BLOCK: test/blkfile_test1.fth
630 AT-BLOCK: test/stack_test.fth
FLUSH
BYE
