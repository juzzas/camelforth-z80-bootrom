\ Forth-2012 test suite
.( Starting Forth 2012 test suite... ) CR

81  LOAD     \ blkfile
303 LOAD     \ ttester
1 2 +THRU










\ Forth-2012 test suite   1 / 2

\ .( Starting preliminary tests... ) CR  330 TLOAD
.( Starting core tests... ) CR         342 TLOAD DECIMAL
\ .( Starting coreplustests ) CR         372 TLOAD
.( Loading test utilities... ) CR      384 TLOAD
.( Loading error reporting... ) CR     390 TLOAD
33 LOAD      \ load core-ext
.( Staring core ext tests... ) CR      396 TLOAD

