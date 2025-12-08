150 LOAD   \ load blkfile
3006 LOAD  \ load tester
.( BLKFILE test suite ) CR
ONLY FORTH ALSO SYSTEM

3605 CONSTANT test-block
3645 CONSTANT ipsum-file
BLK @ 2 + CONSTANT read-block

test-block WIPE FLUSH

\ 3615 INCLUDE-BLKFILE
3630 INCLUDE-BLKFILE



















THIS BLOCK IS FOR READING














END OF BLOCK1
BLK2 IS FOR READING













END OF BLOCK2
