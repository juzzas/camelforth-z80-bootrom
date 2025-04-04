INCLUDE tools/blocks.fth

1000 GENERATE: build/cf-image.blk
OPEN:  build/cf-image.blk
0 BLOCKS: extensions/00system.fth
30 BLOCKS: extensions/tools.fth
60 BLOCKS: extensions/editor.fth
90 BLOCKS: extensions/assembler.fth
120 BLOCKS: extensions/multi.fth
150 BLOCKS: extensions/blkfile.fth
165 BLOCKS: extensions/blkfs.fth
180 BLOCKS: extensions/asmz80.fth
210 BLOCKS: extensions/locals.fth

300 BLOCKS: extensions/forth2012.fth
330 BLOCKS: extensions/string.fth
360 BLOCKS: extensions/core-ext.fth
390 BLOCKS: extensions/double-ext.fth
420 BLOCKS: extensions/tools-ext.fth
450 BLOCKS: extensions/facility-ext.fth
480 BLOCKS: extensions/string-ext.fth
510 BLOCKS: extensions/dyn-memory.fth
540 BLOCKS: extensions/search.fth
570 BLOCKS: extensions/file-access.fth


600 BLOCKS: extensions/double.fth
+BLOCKS: extensions/fixed.fth
+BLOCKS: extensions/random.fth
+BLOCKS: extensions/leds.fth
+BLOCKS: extensions/pagedram.fth
+BLOCKS: extensions/ramdrive.fth

900 BLOCKS: examples/mandelbrot.fth
915 BLOCKS: examples/gamelife.fth
930 BLOCKS: examples/2048.fth
945 BLOCKS: examples/calendar.fth

3000 BLOCKS: test/testsuite.fth
+BLOCKS: test/ttester.fth
+BLKFILE: test/forth2012-test-suite/src/prelimtest.fth
+BLKFILE: test/forth2012-test-suite/src/core.fr
+BLKFILE: test/forth2012-test-suite/src/coreplustest.fth
+BLKFILE: test/forth2012-test-suite/src/utilities.fth
+BLKFILE: test/forth2012-test-suite/src/errorreport.fth
+BLKFILE: test/forth2012-test-suite/src/coreexttest.fth
+BLKFILE: test/forth2012-test-suite/src/blocktest.fth
+BLKFILE: test/forth2012-test-suite/src/doubletest.fth
+BLKFILE: test/forth2012-test-suite/src/exceptiontest.fth
+BLKFILE: test/forth2012-test-suite/src/facilitytest.fth
+BLKFILE: test/forth2012-test-suite/src/filetest.fth
+BLKFILE: test/forth2012-test-suite/src/localstest.fth
+BLKFILE: test/forth2012-test-suite/src/memorytest.fth
+BLKFILE: test/forth2012-test-suite/src/toolstest.fth
+BLKFILE: test/forth2012-test-suite/src/searchordertest.fth
+BLKFILE: test/forth2012-test-suite/src/stringtest.fth

3600 BLOCKS: test/hexload_test.fth
+BLOCKS: test/hexload_test2.fth
+BLOCKS: test/blkfile_test1.fth
+BLOCKS: test/multi_test.fth
FLUSH
BYE
