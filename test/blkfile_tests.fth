CR .( BLKFILE test suite )
   3006 LOAD  \ load tester
   150 LOAD   \ load blkfile
ONLY FORTH ALSO SYSTEM

   3605 CONSTANT test-block   3675 CONSTANT ipsum-file
   3678 CONSTANT test-blkfile-inc
   3679 CONSTANT test-blkfile-nested
   BLK @ 2 + CONSTANT read-block
   3700 CONSTANT test-blkfs   \ test filesystem
   180 CONSTANT test-blkfs%

test-block WIPE FLUSH
3615 LOAD  3630 LOAD  3645 LOAD  \ blkfile tests
180 LOAD   570 LOAD    \ load blkfs
3660 INCLUDE-BLKFILE   \ start blkfs tests
















THIS BLOCK IS FOR READING














END OF BLOCK1
BLK2 IS FOR READING














END OF BLOCK2
