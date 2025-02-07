\ Forth-2012 test suite
.( Starting Forth 2012 test suite... ) CR

81  LOAD     \ blkfile
303 LOAD     \ ttester
1 2 +THRU










\ Forth-2012 test suite   1 / 2

\ .( Starting preliminary tests... ) CR  330 TLOAD
.( Starting core tests... ) CR         342 TLOAD DECIMAL
.( Starting coreplustests ) CR         372 TLOAD
.( Loading test utilities... ) CR      384 TLOAD
.( Loading error reporting... ) CR     390 TLOAD
33 LOAD      \ load core-ext
.( Starting core ext tests... ) CR      396 TLOAD
.( Starting block tests... ) CR      423 TLOAD
51 LOAD     \ load double numbers
.( Starting double tests... ) CR     450 TLOAD
.( Starting exception tests... ) CR     465 TLOAD
.( Starting facility tests... ) CR     471 TLOAD
