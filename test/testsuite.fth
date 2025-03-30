\ Forth-2012 test suite
CR .( Starting Forth 2012 test suite... )

150 LOAD     \ blkfile
165 LOAD     \ blkfs
3006 LOAD     \ ttester
1 2 +THRU









\ Forth-2012 test suite   1 / 2

\ CR .( Starting preliminary tests... )  3015 TLOAD
\ CR .( Starting core tests... )         3027 TLOAD DECIMAL
\ CR .( Starting coreplustests )         3057 TLOAD
30 LOAD      \ load Tools
CR .( Loading test utilities... )        3072 TLOAD
CR .( Loading error reporting... )       3081 TLOAD
300 LOAD     \ load Forth 2012 extenstions
\ CR .( Starting core ext tests... )       3087 TLOAD
\ CR .( Starting block tests... )          3117 TLOAD
\ CR .( Starting double tests... )         3144 TLOAD
\ CR .( Starting exception tests... )      3162 TLOAD
\ CR .( Starting facility tests... )       3168 TLOAD
2000 100 MKFS
2000 ROOT!
CR .( Starting file tests... )           3174 TLOAD
\ CR .( Starting locals tests... )       3186 TLOAD
2048 CONSTANT heap-size
heap-size BUFFER: heap
heap heap-size INIT-HEAP
CR .( Starting memory tests... )         3195 TLOAD
CR .( Starting tools tests... )          3204 TLOAD
CR .( Starting searchorder tests... )    3222 TLOAD
CR .( Starting string tests... )         3231 TLOAD
