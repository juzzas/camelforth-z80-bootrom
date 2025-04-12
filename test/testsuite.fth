\ Forth-2012 test suite
CR .( Starting Forth 2012 test suite... )

150 LOAD     \ blkfile
180 LOAD     \ blkfs
3006 LOAD     \ ttester
1 2 +THRU









\ Forth-2012 test suite   1 / 2

\ CR .( Starting preliminary tests... )  3015 INCLUDE-BLKFILE
\ CR .( Starting core tests... )         3027 INCLUDE-BLKFILE
\ CR .( Starting coreplustests )         3057 INCLUDE-BLKFILE
30 LOAD      \ load Tools
CR .( Loading test utilities... )        3072 INCLUDE-BLKFILE
CR .( Loading error reporting... )       3081 INCLUDE-BLKFILE
300 LOAD     \ load Forth 2012 extenstions
CR .( Starting core ext tests... )       3087 INCLUDE-BLKFILE
\ CR .( Starting block tests... )          3117 INCLUDE-BLKFILE
CR .( Starting double tests... )         3144 INCLUDE-BLKFILE
CR .( Starting exception tests... )      3162 INCLUDE-BLKFILE
CR .( Starting facility tests... )       3168 INCLUDE-BLKFILE
3300 ROOT!
CR .( Starting file tests... )           3174 INCLUDE-BLKFILE
CR .( Starting locals tests... )       3186 INCLUDE-BLKFILE
2048 CONSTANT heap-size
heap-size BUFFER: heap
heap heap-size INIT-HEAP
CR .( Starting memory tests... )         3195 INCLUDE-BLKFILE
\ CR .( Starting tools tests... )          3204 INCLUDE-BLKFILE
CR .( Starting searchorder tests... )    3222 INCLUDE-BLKFILE
CR .( Starting string tests... )         3231 INCLUDE-BLKFILE
