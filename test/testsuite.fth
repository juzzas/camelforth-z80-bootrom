\ Forth-2012 test suite
CR .( Starting Forth 2012 test suite... )

150 LOAD     \ blkfile
3015 LOAD     \ ttester
30 LOAD      \ load Tools
1 11 +THRU

REPORT-ERRORS







\ Forth-2012 test suite   1 / 2
3024 CONSTANT TEST_PRELIM
3036 CONSTANT TEST_CORE
3066 CONSTANT TEST_COREPLUS
3096 CONSTANT TEST_COREEXT
3126 CONSTANT TEST_BLOCK
3153 CONSTANT TEST_DOUBLE
3171 CONSTANT TEST_EXCEPTION
3177 CONSTANT TEST_FACILITY
3183 CONSTANT TEST_FILE
3195 CONSTANT TEST_LOCALS
3204 CONSTANT TEST_MEMORY
3213 CONSTANT TEST_TOOLS
3231 CONSTANT TEST_SEARCHORDER
3240 CONSTANT TEST_STRING

\ CR .( Starting preliminary tests... )  3021 INCLUDE-BLKFILE
CR .( Starting core tests... )       TEST_CORE INCLUDE-BLKFILE
DECIMAL
CR .( Starting coreplustests )   TEST_COREPLUS INCLUDE-BLKFILE

CR .( Loading test utilities... )         3081 INCLUDE-BLKFILE
CR .( Loading error reporting... )        3090 INCLUDE-BLKFILE
300 LOAD     \ load Forth 2012 extenstion wordset loaders

MARKER <reset>
CR .( Starting core ext tests... )
WORDSET_CORE_EXT LOAD
TEST_COREEXT INCLUDE-BLKFILE
CR .( free space: ) UNUSED U.
<reset>

\ test the toolset before we use the compilation wordset
CR .( Starting tools tests... )
MARKER <reset>
WORDSET_TOOLS_EXT LOAD
TEST_TOOLS INCLUDE-BLKFILE
<reset>

[DEFINED] TEST_EXCEPTION [IF]
MARKER <reset>
CR .( Starting exception tests... )
\  WORDSET_EXCEPTION LOAD
TEST_EXCEPTION INCLUDE-BLKFILE
<reset>
[THEN]


[DEFINED] TEST_LOCALS [IF]
   MARKER <reset>
   CR .( Starting locals tests... )
   WORDSET_LOCALS LOAD
   TEST_LOCALS INCLUDE-BLKFILE
   <reset>
[THEN]









[DEFINED] TEST_BLOCK [IF]
   MARKER <reset>
   CR .( Starting block tests... )
   CR .( - loading assembler )
   90 LOAD    \ load assembler
   CR .( - loading pagedram )
   927 LOAD   \ load pagedram wordset
   CR .( - loading ramloader )
   3255 INCLUDE-BLKFILE  \ load ramloader
   TEST_BLOCK  RAMLOAD
   <reset>
[THEN]




[DEFINED] TEST_FILE [IF]
   MARKER <reset>
   CR .( Starting file tests... )
   WORDSET_CORE_EXT LOAD
   WORDSET_FILEACCESS LOAD
   3300 ROOT!
   TEST_FILE INCLUDE-BLKFILE
   <reset>
[THEN]







[DEFINED] TEST_STRING [IF]
   MARKER <reset>
   CR .( Starting string tests... )
   WORDSET_STRING LOAD
   WORDSET_STRING_EXT LOAD
   TEST_STRING INCLUDE-BLKFILE
   <reset>
[THEN]








[DEFINED] TEST_DOUBLE [IF]
   MARKER <reset>
   CR .( Starting double tests... )
   WORDSET_DOUBLE_EXT LOAD
   TEST_DOUBLE INCLUDE-BLKFILE
   <reset>
[THEN]









[DEFINED] TEST_FACILITY [IF]
   MARKER <reset>
   CR .( Starting facility tests... )
   WORDSET_FACILITY_EXT LOAD
   TEST_FACILITY INCLUDE-BLKFILE
   <reset>
[THEN]









[DEFINED] TEST_MEMORY [IF]
   MARKER <reset>
   CR .( Starting memory tests... )
   WORDSET_MEMORY LOAD
   2048 CONSTANT heap-size
   CREATE heap  heap-size ALLOT
   heap heap-size INIT-HEAP
   TEST_MEMORY INCLUDE-BLKFILE
   <reset>
[THEN]






[DEFINED] TEST_SEARCHORDER [IF]
   MARKER <reset>
   CR .( Starting search order tests... )
   WORDSET_SEARCHORDER LOAD
   TEST_SEARCHORDER INCLUDE-BLKFILE
   <reset>
[THEN]



