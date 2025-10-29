150 LOAD   \ load blkfile
3006 LOAD  \ load tester
.( BLKFILE test suite ) CR
ONLY FORTH ALSO SYSTEM

3605 CONSTANT test-block
BLK @ 2 + CONSTANT read-block

test-block WIPE FLUSH

3615 INCLUDE-BLKFILE





















THIS BLOCK IS FOR READING














END OF BLOCK1
BLK2 IS FOR READING













END OF BLOCK2
