\ Forth-2012 test suite
.( Starting Forth 2012 test suite... ) CR

150 LOAD     \ blkfile
3006 LOAD     \ ttester
1 2 +THRU










\ Forth-2012 test suite   1 / 2

\ .( Starting preliminary tests... ) CR  3015 TLOAD
\ .( Starting core tests... ) CR         3027 TLOAD DECIMAL
\ .( Starting coreplustests ) CR         3057 TLOAD
30 LOAD      \ load Tools
.( Loading test utilities... ) CR      3072 TLOAD
.( Loading error reporting... ) CR     3081 TLOAD
303 LOAD     \ load string
309 LOAD     \ load core-ext
324 LOAD     \ load double-ext numbers
339 LOAD     \ load string-ext
363 LOAD    \ search order
\ .( Starting core ext tests... ) CR     3087 TLOAD
\ .( Starting block tests... ) CR        3117 TLOAD
\ .( Starting double tests... ) CR       3144 TLOAD
\ .( Starting exception tests... ) CR    3162 TLOAD
\ .( Starting facility tests... ) CR     3168 TLOAD
\ .( Starting file tests... ) CR     3174 TLOAD
\ .( Starting locals tests... ) CR     3186 TLOAD
\ .( Starting memory tests... ) CR     3195 TLOAD
\ .( Starting tools tests... ) CR     3204 TLOAD
.( Starting searchorder tests... ) CR     3222 TLOAD
.( Starting string tests... ) CR     3231 TLOAD
