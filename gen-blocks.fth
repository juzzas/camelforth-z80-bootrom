INCLUDE tools/blocks.fth
INCLUDE tools/blkfs.fth

4000 GENERATE: build/cf-image.blk
OPEN:  build/cf-image.blk
0 BLOCKS: extensions/00system.fth
30 BLOCKS: extensions/tools.fth
60 BLOCKS: extensions/editor.fth
90 BLOCKS: extensions/assembler.fth
120 BLOCKS: extensions/multi.fth
150 BLOCKS: extensions/blkfile.fth
180 BLOCKS: extensions/blkfs.fth
210 BLOCKS: extensions/asmz80.fth

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
600 BLOCKS: extensions/locals.fth


900 BLOCKS: extensions/double.fth
+BLOCKS: extensions/fixed.fth
+BLOCKS: extensions/random.fth
+BLOCKS: extensions/leds.fth
+BLOCKS: extensions/pagedram.fth
+BLOCKS: extensions/ramdrive.fth

1200 BLOCKS: examples/mandelbrot.fth
1230 BLOCKS: examples/gamelife.fth
1260 BLOCKS: examples/2048.fth
1290 BLOCKS: examples/calendar.fth
1320 BLOCKS: examples/biorhythm.fth
1350 BLOCKS: examples/tetris.fth
1380 BLOCKS: examples/banner.fth

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
+BLKFILE: test/loadfromram.fth

3300 150 MKFS
3300 ROOT!

15 CREAT filetest.fth 
   BLKFILE: test/forth2012-test-suite/src/filetest.fth
3 CREAT required-helper1.fth 
   BLKFILE: test/forth2012-test-suite/src/required-helper1.fth
3 CREAT required-helper2.fth 
   BLKFILE: test/forth2012-test-suite/src/required-helper1.fth

3600 BLOCKS: test/blkfile_tests.fth
3615 BLKFILE: test/blkfile_test1.fth
3630 BLKFILE: test/blkfile_test2.fth
3645 BLKFILE: test/ipsum.txt
3660 BLKFILE: test/blkfs_test.fth

3900 BLOCKS: test/hexload_test.fth
+BLOCKS: test/hexload_test2.fth
+BLOCKS: test/multi_test.fth
FLUSH
BYE
